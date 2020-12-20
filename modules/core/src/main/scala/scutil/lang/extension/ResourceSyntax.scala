package scutil.lang.syntaxes

import scutil.lang._

object ResourceSyntax extends ResourceSyntax

trait ResourceSyntax {
	implicit final class ResourceSyntaxExt[T](peer:T)(implicit RS:Resource[T]) {
		/** do something to us, then dispose */
		def use[U](consume:T=>U):U	= (RS use peer)(consume)
	}
}
