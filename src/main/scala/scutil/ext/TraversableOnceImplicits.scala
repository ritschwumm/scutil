package scutil.ext

object TraversableOnceImplicits extends TraversableOnceImplicits

trait TraversableOnceImplicits {
	implicit def toTraversableOnceExt[T](delegate:TraversableOnce[T])	= new TraversableOnceExt[T](delegate)
}

final class TraversableOnceExt[T](delegate:TraversableOnce[T]) {
	/** 
	return the first Some find creates from elements of this collection
	resembles collectFirst, but uses Function1[_,Option[_]] instead of a PartialFunction[_,_]
	*/
	def flatMapFirst[U](find:T=>Option[U]):Option[U]	= {
		delegate foreach { it =>
			val	out	= find(it)
			if (out.isDefined)	return out
		}
		None
	}
}
