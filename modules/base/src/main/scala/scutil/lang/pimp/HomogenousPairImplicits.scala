package scutil.lang.pimp

import scutil.lang._

object HomogenousPairImplicits extends HomogenousPairImplicits

trait HomogenousPairImplicits {
	implicit final class HomogenousPairExt[T](peer:Tuple2[T,T]) {
		def toSeq:Seq[T]	=
			Seq(peer._1, peer._2)

		def toNes:Nes[T]	=
			Nes.multi(peer._1, peer._2)
	}
}
