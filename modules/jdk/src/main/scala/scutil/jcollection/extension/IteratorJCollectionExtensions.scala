package scutil.jcollection.extension

import java.util.{
	Iterator	as JIterator,
	Enumeration	as JEnumeration
}

object IteratorJCollectionExtensions {
	extension [T](peer:Iterator[T]) {
		def toJIterator:JIterator[T]		= new IteratorAsJIterator(peer)
		def toJEnumeration:JEnumeration[T]	= new IteratorAsJEnumeration(peer)
	}
}
