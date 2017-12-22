package scutil.jcollection.pimp

import java.util.{
	Iterator	=> JIterator,
	Enumeration	=> JEnumeration
}

object IteratorJCollectionSyntaxImplicits extends IteratorJCollectionSyntaxImplicits

trait IteratorJCollectionSyntaxImplicits {
	implicit final class IteratorJCollectionSyntaxExt[T](peer:Iterator[T]) {
		def toJIterator:JIterator[T]		= new IteratorAsJIterator(peer)
		def toJEnumeration:JEnumeration[T]	= new IteratorAsJEnumeration(peer)
	}
}
