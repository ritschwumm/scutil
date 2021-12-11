package scutil.jcollection.extension

import java.lang.{
	Iterable	as JIterable
}

object JIterableExtensions {
	implicit final class JIterableExt[T](peer:JIterable[T]) {
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
