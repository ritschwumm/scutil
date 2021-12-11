package scutil.jcollection.extension

import java.util.{ Set => JSet }

import scala.collection.immutable.HashSet

object JSetImplicits {
	implicit final class JSetExt[T](peer:JSet[T]) {
		def toHashSet:HashSet[T]	= toIterable.to(HashSet)
		def toSet:Set[T]			= toHashSet
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
