package scutil.io.extension

import java.nio.file.*

import scutil.core.implicits.*

object PathExtensions {
	/** utility methods for java Path objects */
	implicit final class PathExt(peer:Path) {
		/** add a component to this Path */
		def /(name:String):Path		= peer.resolve(name)

		/** add multiple components to this Paths */
		def /+(path:Seq[String]):Path	= path.foldLeft(peer)(_ `resolve` _)

		/** prefer this over toString */
		def getPathString:String	= peer.toString

		/** the unix root has no file name */
		def getFileNameString:Option[String]	=
			fileNameOption.map(_.toString)

		/** the unix root has no file name */
		def fileNameOption:Option[Path]	=
			Option(peer.getFileName)

		def rootOption:Option[Path]	=
			Option(peer.getRoot)

		/** get the parent Path of None if nt possible */
		def parentOption:Option[Path]	=
			Option(peer.getParent)

		def nameOption(index:Int):Option[Path]	=
			if (index >= 0 && index < peer.getNameCount)	Some(peer.getName(index))
			else											None

		/** get a sub path or None if not possible */
		def subpathOption(beginIndex:Int, endIndex:Int):Option[Path]	=
			if (
				beginIndex >= 0 && beginIndex < peer.getNameCount &&
				endIndex > beginIndex &&
				endIndex <= peer.getNameCount
			) {
				Some(peer.subpath(beginIndex, endIndex))
			}
			else None

		/** get all parent Files starting with the immediate parent and ending with the directory root */
		def parentChain:List[Path]	=
			List.unfoldSimple(peer) { it => Option(it.getParent) }

		/** like parentChain but includes the starting file itself */
		def selfAndParentChain:List[Path]	=
			peer :: parentChain

		/** the path upwards from another Path to this Path */
		def containsRecursive(that:Path):Option[Seq[String]]	= {
			def loop(test:Path, path:Seq[String]):Option[Seq[String]]	=
				if		(test == null)	None
				else if	(test == peer)	Some(path)
				// TODO path check using an empty string here makes sense
				else					loop(test.getParent, Option(test.getFileName).map(_.toString).getOrElse("") +: path)
			loop(that, Vector.empty)
		}

		/** map only the name of this Path */
		def siblingBy(func:String=>String):Path =
			// TODO path throw an NPE if getFileName returns null
			peer.resolveSibling(func(peer.getFileName.toString))
	}
}
