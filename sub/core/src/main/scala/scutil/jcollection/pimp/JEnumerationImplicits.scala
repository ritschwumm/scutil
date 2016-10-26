package scutil.jcollection.pimp

import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

object JEnumerationImplicits extends JEnumerationImplicits

trait JEnumerationImplicits {
	implicit def toJEnumerationExt[T](peer:JEnumeration[T])	= new JEnumerationExt(peer)
}

final class JEnumerationExt[T](peer:JEnumeration[T]) {
	def toIterator:Iterator[T] 		= new JEnumerationAsIterator(peer)
	def toJIterator:JIterator[T]	= new IteratorAsJIterator(toIterator)
}
