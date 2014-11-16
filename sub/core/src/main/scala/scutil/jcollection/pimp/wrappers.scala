package scutil.jcollection.pimp

import java.lang.{
	Iterable	=> JIterable 
}
import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

import scala.collection.AbstractIterable
import scala.collection.AbstractIterator

final class JIterableAsIterable[T](peer:JIterable[T]) extends AbstractIterable[T] {
	def iterator:Iterator[T]	= new JIteratorAsIterator(peer.iterator)
}

final class JIteratorAsIterator[T](peer:JIterator[T]) extends AbstractIterator[T] {
	def hasNext:Boolean	= peer.hasNext
	def next():T		= peer.next()
}

final class JEnumerationAsIterator[T](peer:JEnumeration[T]) extends AbstractIterator[T] {
	def hasNext:Boolean	= peer.hasMoreElements
	def next():T		= peer.nextElement()
}
