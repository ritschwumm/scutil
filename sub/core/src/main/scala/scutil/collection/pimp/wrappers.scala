package scutil.collection.pimp

import java.lang.{
	Iterable	=> JIterable
}
import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

final class IterableAsJIterable[T](peer:Iterable[T]) extends JIterable[T] {
	def iterator:JIterator[T]	= new IteratorAsJIterator(peer.iterator)
}

final class IteratorAsJIterator[T](peer:Iterator[T]) extends JIterator[T] {
	def hasNext:Boolean	= peer.hasNext
	def next():T		= peer.next()
	def remove():Unit	= throw new UnsupportedOperationException()
}

final class IteratorAsJEnumeration[T](peer:Iterator[T]) extends JEnumeration[T] {
	def hasMoreElements:Boolean	= peer.hasNext
	def nextElement():T			= peer.next()
}
