package scutil.jcollection.extension

import java.util.{
	Iterator	as JIterator,
	Enumeration	as JEnumeration
}

object JEnumerationExtensions {
	extension [T](peer:JEnumeration[T]) {
		def toIterator:Iterator[T]		= new JEnumerationAsIterator(peer)
		def toJIterator:JIterator[T]	= new IteratorAsJIterator(toIterator)
	}
}
