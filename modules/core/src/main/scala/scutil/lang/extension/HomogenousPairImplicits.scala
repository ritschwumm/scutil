package scutil.lang.extension

import scutil.lang._

object HomogenousPairImplicits extends HomogenousPairImplicits

trait HomogenousPairImplicits {
	implicit final class HomogenousPairExt[T](peer:Tuple2[T,T]) {
		@deprecated("will be removed", "0.197.0")
		def toSeq:Seq[T]	=
			Seq(peer._1, peer._2)

		@deprecated("will be removed", "0.197.0")
		def toNes:Nes[T]	=
			Nes.of(peer._1, peer._2)
	}
}
