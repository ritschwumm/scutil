package scutil.jcollection.extension

import java.lang.{
	Iterable	as JIterable
}
import java.util.{
	Iterator	as JIterator,
	Enumeration	as JEnumeration
}

import scala.collection.AbstractIterable
import scala.collection.AbstractIterator

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

final class IterableAsJIterable[T](peer:Iterable[T]) extends JIterable[T] {
	def iterator:JIterator[T]	= new IteratorAsJIterator(peer.iterator)
}

final class IteratorAsJIterator[T](peer:Iterator[T]) extends JIterator[T] {
	def hasNext:Boolean	= peer.hasNext
	def next():T		= peer.next()
	override def remove():Unit	= throw new UnsupportedOperationException()
}

final class IteratorAsJEnumeration[T](peer:Iterator[T]) extends JEnumeration[T] {
	def hasMoreElements:Boolean	= peer.hasNext
	def nextElement():T			= peer.next()
}
