package scutil.collection.pimp

import scutil.lang._

object TraversableOnceImplicits extends TraversableOnceImplicits

trait TraversableOnceImplicits {
	implicit def toTraversableOnceExt[T](peer:TraversableOnce[T])	= new TraversableOnceExt[T](peer)
}

final class TraversableOnceExt[T](peer:TraversableOnce[T]) {
	/** 
	return the first Some find creates from elements of this collection
	resembles collectFirst, but uses Function1[_,Option[_]] instead of a PartialFunction[_,_]
	*/
	def collapseFirst[U](find:PFunction[T,U]):Option[U]	= {
		peer foreach { it =>
			val	out	= find(it)
			if (out.isDefined)	return out
		}
		None
	}
}
