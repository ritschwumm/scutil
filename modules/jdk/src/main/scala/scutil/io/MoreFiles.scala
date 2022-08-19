package scutil.io

import java.util.EnumSet
import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.*

import scutil.time.*

object MoreFiles {
	def optionExists(file:Path):Option[Path]	=
		if (Files.exists(file))	Some(file)
		else					None

	// TODO path provide an Ordering for FileTime
	// TODO path provide conversion from and toi MilliInstant for FileTime

	/** time of last modification as an MilliInstant, returns MilliInstant.zero for non-existing files */
	def lastModified(file:Path):MilliInstant	=
		MilliInstant(Files.getLastModifiedTime(file).toMillis)

	def setLastModified(file:Path, time:MilliInstant):Unit	=
		Files.setLastModifiedTime(file, FileTime.fromMillis(time.millis))

	/** whether the candidate is newer than another file. if the other file does not exist it counts as newer */
	def newerThan(candidate:Path, against:Path):Boolean	=
		Files.exists(candidate) && (!Files.exists(against) || Files.getLastModifiedTime(candidate).toMillis > Files.getLastModifiedTime(against).toMillis)

	def createParentDirectories(file:Path):Unit	=
		Option(file.getParent).foreach { parent =>
			Files.createDirectories(parent)
		}

	def deleteRecursive(directory:Path):Unit	=
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
						if (predicate(selectedPath)) {
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
}
