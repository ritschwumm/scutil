package scutil.jcollection.pimp

import java.util.{ Set => JSet }

import scala.collection.immutable.HashSet

object JSetImplicits extends JSetImplicits

trait JSetImplicits {
	implicit def toJSetExt[T](peer:JSet[T])	= new JSetExt(peer)
}

final class JSetExt[T](peer:JSet[T]) {
	def toHashSet:HashSet[T]	= toIterable.to[HashSet]
	def toSet:Set[T]			= toHashSet
	def toIterable:Iterable[T]	= new JIterableWrapper(peer)
}
