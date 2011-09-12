package scutil.ext

object TraversableOnceImplicits extends TraversableOnceImplicits

trait TraversableOnceImplicits {
	implicit def toTraversableOnceExt[T](delegate:TraversableOnce[T])	= new TraversableOnceExt[T](delegate)
}

final class TraversableOnceExt[T](delegate:TraversableOnce[T]) {
	def toVector:Vector[T]	= Vector.empty ++ delegate
}
