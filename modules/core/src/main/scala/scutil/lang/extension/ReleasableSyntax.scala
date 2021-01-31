package scutil.lang.syntaxes

import scala.util.Using.Releasable

import scutil.lang._

object ReleaseableSyntax extends ReleaseableSyntax

trait ReleaseableSyntax {
	implicit final class ReleaseableSyntaxExt[T](peer:T)(implicit RS:Releasable[T]) {
		/** do something to us, then dispose */
		def use[U](consume:T=>U):U	=
			Using.unsafeBracket(peer)(RS.release)(consume)
	}
}
