package scutil.io

import java.util.EnumSet
import java.io.*
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.*
import java.nio.file.attribute.*
import java.nio.charset.Charset

import scala.jdk.CollectionConverters.*

import scutil.core.implicits.*
import scutil.lang.ByteString
import scutil.time.*

object MoreFiles {
	def optionExists(path:Path):Option[Path]	=
		if (Files.exists(path))	Some(path)
		else					None

	//------------------------------------------------------------------------------
	//## modification time

	/** time of last modification as an MilliInstant, returns MilliInstant.zero for non-existing files */
	def lastModified(path:Path):MilliInstant	=
		fileTimeToMilliInstant(Files.getLastModifiedTime(path))

	def setLastModified(path:Path, time:MilliInstant):Unit	=
		Files.setLastModifiedTime(path, milliInstantToFileTime(time))

	def fileTimeToMilliInstant(it:FileTime):MilliInstant	= MilliInstant(it.toMillis)
	def milliInstantToFileTime(it:MilliInstant):FileTime	= FileTime.fromMillis(it.millis)

	/** whether the candidate is newer than another file. if the other file does not exist it counts as newer */
	def newerThan(candidate:Path, against:Path):Boolean	=
		Files.exists(candidate) && (
			!Files.exists(against) ||
			Files.getLastModifiedTime(candidate).toMillis > Files.getLastModifiedTime(against).toMillis
		)

	//------------------------------------------------------------------------------
	//## content materialized

	def readByteString(file:Path):ByteString							= ByteString.unsafeFromArray(Files.readAllBytes(file))
	def writeByteString(file:Path, bytes:ByteString):Unit				= Files.write(file, bytes.unsafeValue)

	def readString(file:Path, charset:Charset):String					= new String(Files.readAllBytes(file), charset)
	def writeString(file:Path, charset:Charset, string:String):Unit		= Files.write(file, string.getBytes(charset))

	def readLines(file:Path, charset:Charset):Seq[String]				= Files.readAllLines(file, charset).asScala.to(Vector)
	def writeLines(file:Path, charset:Charset, lines:Seq[String]):Unit	= Files.write(file, lines.asJava, charset)

	//------------------------------------------------------------------------------
	//## content streaming

	/** execute a closure with an InputStream reading from this File */
	def withInputStream[T](file:Path)(code:(InputStream=>T)):T	=
		Files.newInputStream(file).use(code)

	/** execute a closure with an OutputStream writing into this File */
	def withOutputStream[T](file:Path)(code:(OutputStream=>T)):T	=
		Files.newOutputStream(file).use(code)

	/** execute a closure with a Reader reading from this File */
	def withReader[T](file:Path, charset:Charset)(code:(Reader=>T)):T	=
		Files.newBufferedReader(file, charset).use(code)

	/** execute a closure with a Writer writing into this File */
	def withWriter[T](file:Path, charset:Charset)(code:(Writer=>T)):T	=
		Files.newBufferedWriter(file, charset).use(code)

	//------------------------------------------------------------------------------
	//## content mapped

	def mapReadOnly(file:Path):MappedByteBuffer	=
		new RandomAccessFile(file.toFile, "r").getChannel use { fc =>
			fc.map(FileChannel.MapMode.READ_ONLY, 0, Files.size(file))
		}

	//------------------------------------------------------------------------------
	//## structure

	def createParentDirectories(path:Path):Unit	=
		Option(path.getParent).foreach { parent =>
			Files.createDirectories(parent)
		}

	/** does not follow symlinks */
	def deleteRecursive(directory:Path):Unit	=
		Files.walkFileTree(
			directory,
			new SimpleFileVisitor[Path] {
				override def visitFile(file:Path, attrs:BasicFileAttributes):FileVisitResult	= {
					Files.delete(file)
					FileVisitResult.CONTINUE
				}

				override def postVisitDirectory(dir:Path, e:IOException):FileVisitResult	= {
					if (e != null)	throw e
					Files.delete(dir)
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
				EnumSet.of(FileVisitOption.FOLLOW_LINKS),
				1,
				new SimpleFileVisitor[Path] {
					override def preVisitDirectory(dir:Path, attrs:BasicFileAttributes)	= {
						if (dir != directory && predicate(dir)) {
							out	= out :+ dir
						}
						FileVisitResult.CONTINUE
					}

					override def visitFile(file:Path, attrs:BasicFileAttributes)	= {
						if (predicate(file)) {
							out	= out :+ file
						}
						FileVisitResult.CONTINUE
					}
				}
			)

			Some(out)
		}
		else None
}
