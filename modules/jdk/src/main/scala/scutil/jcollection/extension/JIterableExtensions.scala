package scutil.jcollection.extension

import java.lang.{
	Iterable	as JIterable
}

object JIterableExtensions {
	extension [T](peer:JIterable[T]) {
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
