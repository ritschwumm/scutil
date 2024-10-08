package scutil.jcollection.extension

import java.util.{
	Iterator	as JIterator,
	Enumeration	as JEnumeration
}

object JIteratorExtensions {
	extension [T](peer:JIterator[T]) {
		def toIterator:Iterator[T]			= new JIteratorAsIterator(peer)
		def toJEnumeration:JEnumeration[T]	= new IteratorAsJEnumeration(toIterator)
	}
}
