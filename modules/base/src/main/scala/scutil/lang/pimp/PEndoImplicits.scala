package scutil.lang.pimp

import scutil.lang._

object PEndoImplicits extends PEndoImplicits

trait PEndoImplicits {
	implicit final class PEndoExt[T](peer:PEndo[T]) {
		def applyOrOriginal(it:T):T	=
				peer(it) getOrElse it

		def orOriginal:Endo[T]	=
				it => peer(it) getOrElse it
	}
}
