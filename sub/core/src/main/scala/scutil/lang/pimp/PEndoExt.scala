package scutil.lang.pimp

import scutil.lang._

object PEndoImplicits extends PEndoImplicits

trait PEndoImplicits {
	implicit def toPEndoExt[T](peer:PEndo[T])	= new PEndoExt[T](peer)
}

final class PEndoExt[T](peer:PEndo[T]) {
	def orOriginal:Endo[T]	=
			t => peer(t) getOrElse t
}
