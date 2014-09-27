package scutil.collection.pimp

import java.lang.{ Iterable => JIterable }
import java.util.{ Iterator => JIterator }

import scala.collection.AbstractIterable
import scala.collection.AbstractIterator

private[pimp] final class IteratorWrapper[T](peer:Iterator[T]) extends JIterator[T] {
	def hasNext:Boolean	= peer.hasNext
	def next():T		= peer.next()
}

private[pimp] final class IterableWrapper[T](peer:Iterable[T]) extends JIterable[T] {
	def iterator:JIterator[T]	= new IteratorWrapper(peer.iterator)
}
