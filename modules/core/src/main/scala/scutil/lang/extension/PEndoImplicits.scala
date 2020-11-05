package scutil.lang.extension

object PEndoImplicits extends PEndoImplicits

trait PEndoImplicits {
	implicit final class PEndoExt[T](peer:T=>Option[T]) {
		def applyOrOriginal(it:T):T	= peer(it) getOrElse it
		def orOriginal:T=>T			= it => peer(it) getOrElse it
	}
}
