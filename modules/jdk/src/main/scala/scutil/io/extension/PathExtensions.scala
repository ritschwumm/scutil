package scutil.io.extension

import java.io.*
import java.nio.file.*
import java.nio.file.attribute.FileTime
import java.nio.charset.Charset

import scutil.core.implicits.*
import scutil.io.MoreFiles
import scutil.lang.ByteString

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

		@deprecated("use MoreFiles.withInputStream", "0.229.0")
		def withInputStream[T](code:(InputStream=>T)):T	=
			MoreFiles.withInputStream(peer)(code)

		@deprecated("use MoreFiles.withOutputStream", "0.229.0")
		def withOutputStream[T](code:(OutputStream=>T)):T	=
			MoreFiles.withOutputStream(peer)(code)

		@deprecated("use MoreFiles.withReader", "0.229.0")
		def withReader[T](charset:Charset)(code:(Reader=>T)):T	=
			MoreFiles.withReader(peer, charset)(code)

		@deprecated("use MoreFiles.withWriter", "0.229.0")
		def withWriter[T](charset:Charset)(code:(Writer=>T)):T	=
			MoreFiles.withWriter(peer, charset)(code)

		//------------------------------------------------------------------------------
		//## file only: complete read

		@deprecated("use MoreFiles.readByteString", "0.229.0")
		def readByteString():ByteString							= MoreFiles.readByteString(peer)

		@deprecated("use MoreFiles.writeByteString", "0.229.0")
		def writeByteString(bytes:ByteString):Unit				= MoreFiles.writeByteString(peer, bytes)

		@deprecated("use MoreFiles.readString", "0.229.0")
		def readString(charset:Charset):String					= MoreFiles.readString(peer, charset)

		@deprecated("use MoreFiles.writeString", "0.229.0")
		def writeString(charset:Charset, string:String):Unit	= MoreFiles.writeString(peer, charset, string)

		@deprecated("use MoreFiles.readLines", "0.229.0")
		def readLines(charset:Charset):Seq[String]				= MoreFiles.readLines(peer, charset)

		@deprecated("use MoreFiles.writeLines", "0.229.0")
		def writeLines(charset:Charset, lines:Seq[String]):Unit	= MoreFiles.writeLines(peer, charset, lines)
	}
}
