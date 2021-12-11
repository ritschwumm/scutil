package scutil.jcollection.extension

import java.lang.{
	Iterable	=> JIterable
}

object JIterableExtensions {
	implicit final class JIterableExt[T](peer:JIterable[T]) {
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
