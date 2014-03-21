package scutil.collection.pimp

import java.util.concurrent.Callable

object IteratorImplicits extends IteratorImplicits

trait IteratorImplicits {
	implicit def toIteratorExt[T](peer:Iterator[T]) = new IteratorExt[T](peer)
}

final class IteratorExt[T](peer:Iterator[T]) {
	def nextOption:Option[T]	=
			if (peer.hasNext)	Some(peer.next)
			else				None
}
