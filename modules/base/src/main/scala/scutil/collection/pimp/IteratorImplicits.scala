package scutil.collection.pimp

object IteratorImplicits extends IteratorImplicits

trait IteratorImplicits {
	implicit final class IteratorExt[T](peer:Iterator[T]) {
		def nextOption:Option[T]	=
				if (peer.hasNext)	Some(peer.next)
				else				None
	}
}
