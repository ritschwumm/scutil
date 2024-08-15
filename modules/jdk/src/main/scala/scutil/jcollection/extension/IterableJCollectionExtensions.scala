package scutil.jcollection.extension

import java.lang.{
	Iterable	as JIterable
}

object IterableJCollectionExtensions {
	extension [T](peer:Iterable[T]) {
		def toJIterable:JIterable[T]	= new IterableAsJIterable(peer)
	}
}
