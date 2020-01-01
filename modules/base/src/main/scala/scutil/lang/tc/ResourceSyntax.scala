package scutil.lang.tc

object ResourceSyntax extends ResourceSyntax

trait ResourceSyntax {
	implicit class ResourceSyntaxExt[T](peer:T)(implicit RS:Resource[T]) {
		/** do something to us, then dispose */
		def use[U](consume:T=>U):U	= (RS use peer)(consume)
	}
}
