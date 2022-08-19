package scutil.io

import java.util.EnumSet
import java.io.IOException
import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.*
import java.nio.file.attribute.*

import scutil.core.implicits.*
import scutil.time.*

object MoreFiles {
	def optionExists(file:Path):Option[Path]	=
		if (Files.exists(file))	Some(file)
		else					None

	/** time of last modification as an MilliInstant, returns MilliInstant.zero for non-existing files */
	def lastModified(file:Path):MilliInstant	=
		fileTimeToMilliInstant(Files.getLastModifiedTime(file))

	def setLastModified(file:Path, time:MilliInstant):Unit	=
		Files.setLastModifiedTime(file, milliInstantToFileTime(time))

	def fileTimeToMilliInstant(it:FileTime):MilliInstant	= MilliInstant(it.toMillis)
	def milliInstantToFileTime(it:MilliInstant):FileTime	= FileTime.fromMillis(it.millis)

	/** whether the candidate is newer than another file. if the other file does not exist it counts as newer */
	def newerThan(candidate:Path, against:Path):Boolean	=
		Files.exists(candidate) && (!Files.exists(against) || Files.getLastModifiedTime(candidate).toMillis > Files.getLastModifiedTime(against).toMillis)

	def createParentDirectories(file:Path):Unit	=
		Option(file.getParent).foreach { parent =>
			Files.createDirectories(parent)
		}

	/** does not follow symlinks */
	def deleteRecursive(directory:Path, followSymLinks:Boolean):Unit	=
		Files.walkFileTree(
			directory,
			new SimpleFileVisitor[Path] {
				override def visitFile(file:Path, attrs:BasicFileAttributes):FileVisitResult	= {
					Files delete file
					FileVisitResult.CONTINUE
				}
				override def postVisitDirectory(dir:Path, e:IOException):FileVisitResult	= {
					if (e != null)	throw e
					Files delete dir
					FileVisitResult.CONTINUE
				}
			}
		)

	/** list files in this directory */
	def listFiles(directory:Path):Option[Vector[Path]]	=
		listFilesWhere(directory, _ => true)

	/** list files in this directory */
	def listFilesWhere(directory:Path, predicate:Path=>Boolean):Option[Vector[Path]]	=
		if (Files.isDirectory(directory)) {
			var	out	= Vector.empty[Path]

			Files.walkFileTree(
				directory,
				EnumSet noneOf classOf[FileVisitOption],
				1,
				new SimpleFileVisitor[Path] {
					override def preVisitDirectory(selectedPath:Path, attrs:BasicFileAttributes)	= {
						if (selectedPath != directory && predicate(selectedPath)) {
							out	= out :+ selectedPath
						}
						FileVisitResult.CONTINUE
					}

					override def visitFile(selectedPath:Path, attrs:BasicFileAttributes)	= {
						if (predicate(selectedPath)) {
							out	= out :+ selectedPath
						}
						FileVisitResult.CONTINUE
					}
				}
			)

			Some(out)
		}
		else None

	def mapReadOnly(file:Path):MappedByteBuffer	=
		new RandomAccessFile(file.toFile, "r").getChannel use { fc =>
			fc.map(FileChannel.MapMode.READ_ONLY, 0, Files.size(file))
		}
}
