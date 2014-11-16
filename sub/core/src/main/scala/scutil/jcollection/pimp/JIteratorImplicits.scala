package scutil.jcollection.pimp

import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

import scutil.collection.pimp.IteratorAsJEnumeration

object JIteratorImplicits extends JIteratorImplicits

trait JIteratorImplicits {
	implicit def toJIteratorExt[T](peer:JIterator[T])	= new JIteratorExt(peer)
}

final class JIteratorExt[T](peer:JIterator[T]) {
	def toIterator:Iterator[T] 			= new JIteratorAsIterator(peer)
	def toJEnumeration:JEnumeration[T]	= new IteratorAsJEnumeration(toIterator)
}
