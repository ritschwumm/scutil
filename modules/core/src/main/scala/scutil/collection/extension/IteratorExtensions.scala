package scutil.collection.extension

object IteratorExtensions {
	extension [T](peer:Iterator[T]) {
		def nextOption:Option[T]	=
			if (peer.hasNext)	Some(peer.next())
			else				None
	}
}
