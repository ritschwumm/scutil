package scutil.jcollection.extension

import java.lang.{
	Iterable	as JIterable
}

object IterableJCollectionExtensions {
	implicit final class IterableJCollectionSyntaxExt[T](peer:Iterable[T]) {
		def toJIterable:JIterable[T]	= new IterableAsJIterable(peer)
	}
}
