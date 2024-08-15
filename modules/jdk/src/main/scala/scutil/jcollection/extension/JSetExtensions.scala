package scutil.jcollection.extension

import java.util.{ Set as JSet }

import scala.collection.immutable.HashSet

object JSetExtensions {
	extension [T](peer:JSet[T]) {
		def toHashSet:HashSet[T]	= toIterable.to(HashSet)
		def toSet:Set[T]			= toHashSet
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
