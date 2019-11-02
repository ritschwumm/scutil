package scutil.collection.pimp

import scutil.lang.ISeq

object IteratorImplicits extends IteratorImplicits

trait IteratorImplicits {
	implicit final class IteratorExt[T](peer:Iterator[T]) {
		def nextOption:Option[T]	=
				if (peer.hasNext)	Some(peer.next)
				else				None

		@deprecated("use toSeq", "0.162.0")
		def toISeq:ISeq[T]	=
			toSeq

		// TODO is this still necessary with scala 2.12?
		def toSeq:Seq[T]	= peer.toVector
	}
}
