package scutil.io.extension

import java.nio.file.Path

object PathImplicits extends PathImplicits

trait PathImplicits {
	/** utility methods for java Path objects */
	implicit final class PathExt(peer:Path) {
		/** add a component to this Paths's path */
		def /(name:String):Path 		= peer resolve name

		/** add multiple components to this Paths's path */
		def /+(path:Seq[String]):Path	= (path foldLeft peer) { _ resolve _ }
	}
}
