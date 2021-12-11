package scutil.jcollection.extension

import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

object JEnumerationImplicits {
	implicit final class JEnumerationExt[T](peer:JEnumeration[T]) {
		def toIterator:Iterator[T] 		= new JEnumerationAsIterator(peer)
		def toJIterator:JIterator[T]	= new IteratorAsJIterator(toIterator)
	}
}
