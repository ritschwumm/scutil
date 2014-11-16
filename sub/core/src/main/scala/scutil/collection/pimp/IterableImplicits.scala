package scutil.collection.pimp

import java.lang.{
	Iterable	=> JIterable
}

import scutil.lang.ISeq

object IterableImplicits extends IterableImplicits

trait IterableImplicits {
	implicit def toIterableExt[T](peer:Iterable[T]) = new IterableExt[T](peer)
}

final class IterableExt[T](peer:Iterable[T]) {
	def toISeq:ISeq[T]				= peer.toVector
	
	def toJIterable:JIterable[T]	= new IterableAsJIterable(peer)
}
