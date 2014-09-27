package scutil.jcollection.pimp

import java.lang.{ Iterable => JIterable }
import java.util.{ Iterator => JIterator }

import scala.collection.AbstractIterable
import scala.collection.AbstractIterator

private[pimp] final class JIteratorWrapper[T](peer:JIterator[T]) extends AbstractIterator[T] {
	def hasNext:Boolean	= peer.hasNext
	def next():T		= peer.next()
}

private[pimp] final class JIterableWrapper[T](peer:JIterable[T]) extends AbstractIterable[T] {
	def iterator:Iterator[T]	= new JIteratorWrapper(peer.iterator)
}
