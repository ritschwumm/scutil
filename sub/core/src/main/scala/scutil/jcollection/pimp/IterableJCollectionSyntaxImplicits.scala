package scutil.jcollection.pimp

import java.lang.{
	Iterable	=> JIterable
}

object IterableJCollectionSyntaxImplicits extends IterableJCollectionSyntaxImplicits

trait IterableJCollectionSyntaxImplicits {
	implicit def toIterableJCollectionSyntaxExt[T](peer:Iterable[T]) = new IterableJCollectionSyntaxExt(peer)
}

final class IterableJCollectionSyntaxExt[T](peer:Iterable[T]) {
	def toJIterable:JIterable[T]	= new IterableAsJIterable(peer)
}
