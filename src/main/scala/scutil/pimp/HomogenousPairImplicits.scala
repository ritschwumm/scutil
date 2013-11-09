package scutil.pimp

import scutil.lang._

object HomogenousPairImplicits extends HomogenousPairImplicits

trait HomogenousPairImplicits {
	implicit def toHomogenousPairExt[T](peer:Pair[T,T]) = new HomogenousPairExt[T](peer)
}

final class HomogenousPairExt[T](peer:Pair[T,T]) {
	def toSeq:Seq[T]	=
			Seq(peer._1, peer._2)
	
	def toNes:Nes[T]	= 
			Nes multi (peer._1, peer._2)
}
