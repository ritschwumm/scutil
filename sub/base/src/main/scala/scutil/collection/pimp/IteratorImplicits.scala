package scutil.collection.pimp

import scutil.lang.ISeq

object IteratorImplicits extends IteratorImplicits

trait IteratorImplicits {
	implicit def toIteratorExt[T](peer:Iterator[T]) = new IteratorExt[T](peer)
}

final class IteratorExt[T](peer:Iterator[T]) {
	def nextOption:Option[T]	=
			if (peer.hasNext)	Some(peer.next)
			else				None
		
	def toISeq:ISeq[T]	= peer.toVector
}
