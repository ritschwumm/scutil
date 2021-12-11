package scutil.collection.extension

object IteratorExtensions {
	implicit final class IteratorExt[T](peer:Iterator[T]) {
		def nextOption:Option[T]	=
			if (peer.hasNext)	Some(peer.next())
			else				None
	}
}
