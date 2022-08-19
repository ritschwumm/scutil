package scutil.io.extension

import java.io.*
import java.nio.file.*
import java.nio.file.attribute.FileTime
import java.nio.charset.Charset

import scutil.core.implicits.*
import scutil.io.implicits.*
import scutil.io.MoreFiles
import scutil.lang.*
import scutil.time.*
import scutil.platform.SystemProperties

import InputStreamExtensions.*
import OutputStreamExtensions.*
import ReaderExtensions.*
import WriterExtensions.*

object PathExtensions {
	/** utility methods for java Path objects */
	implicit final class PathExt(peer:Path) {
		//------------------------------------------------------------------------------
		//## pure path manipulation

		/** add a component to this Paths's path */
		def /(name:String):Path 		= peer resolve name

		/** add multiple components to this Paths's path */
		def /+(path:Seq[String]):Path	= path.foldLeft(peer)(_ resolve _)

		/** prefer this over toString */
		def getPathString:String	= peer.toString

		/** the unix root has no file name */
		def getFileNameString:Option[String]	= Option(peer.getFileName).map(_.toString)

		/** get the parent Path the scala way */
		def parentOption:Option[Path]	=
			Option(peer.getParent)

		/** get all parent Files starting with the immediate parent and ending with the directory root */
		def parentChain:List[Path]	=
			List.unfoldSimple(peer) { it => Option(it.getParent) }

		/** like parentChain but includes the starting file itself */
		def selfAndParentChain:List[Path]	=
			peer :: parentChain

		/** the path upwards from another File to this File */
		def containsRecursive(that:Path):Option[Seq[String]]	= {
			def loop(test:Path, path:Seq[String]):Option[Seq[String]]	=
				if		(test == null)	None
				else if	(test == peer)	Some(path)
				// TODO path check using an empty string here makes sense
				else					loop(test.getParent, Option(test.getFileName).map(_.toString).getOrElse("") +: path)
			loop(that, Vector.empty)
		}

		/** another path in the same parent directory */
		def sibling(name:String):Path	=
			peer.getParent.resolve(name)

		/** map only the name of this Path */
		def siblingBy(func:String=>String):Path =
			sibling(func(peer.getFileName.toString))

		//------------------------------------------------------------------------------
		//## file only: resource closure

		// BETTER handle IOException

		/** execute a closure with an InputStream reading from this File */
		def withInputStream[T](code:(InputStream=>T)):T	=
			Files.newInputStream(peer) use code

		/** execute a closure with an OutputStream writing into this File */
		def withOutputStream[T](code:(OutputStream=>T)):T	=
			Files.newOutputStream(peer) use code

		/** execute a closure with a Reader reading from this File */
		def withReader[T](charset:Charset)(code:(InputStreamReader=>T)):T	=
			new InputStreamReader(Files.newInputStream(peer), charset) use code

		/** execute a closure with a Writer writing into this File */
		def withWriter[T](charset:Charset)(code:(OutputStreamWriter=>T)):T	=
			new OutputStreamWriter(Files.newOutputStream(peer), charset) use code

		//------------------------------------------------------------------------------
		//## file only: complete read

		// BETTER use this?
		//def readByteString():ByteString				= ByteString unsafeFromArray (Files readAllBytes peer.toPath)
		//def writeByteString(bytes:ByteString):Unit	= Files write (peer.toPath, bytes.unsafeValue)

		def readByteString():ByteString				= withInputStream	{ _.readFullyByteString()	}
		def writeByteString(bytes:ByteString):Unit	= withOutputStream	{ _ writeByteString bytes	}

		def readString(charset:Charset):String					= withReader(charset) { _.readFully() }
		def writeString(charset:Charset, string:String):Unit	= withWriter(charset) { _ write string }

		// BETTER use a specific line separator
		def readLines(charset:Charset):Seq[String]	=
			withReader(charset) { _.readLines() }
		def writeLines(charset:Charset, lines:Seq[String]):Unit	=
			withWriter(charset) { writer =>
				lines foreach { line =>
					writer write line
					writer write SystemProperties.line.separator
				}
			}
	}
}
