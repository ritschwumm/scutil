package scutil.jcollection.extension

import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

object JIteratorImplicits extends JIteratorImplicits

trait JIteratorImplicits {
	implicit final class JIteratorExt[T](peer:JIterator[T]) {
		def toIterator:Iterator[T] 			= new JIteratorAsIterator(peer)
		def toJEnumeration:JEnumeration[T]	= new IteratorAsJEnumeration(toIterator)
	}
}
