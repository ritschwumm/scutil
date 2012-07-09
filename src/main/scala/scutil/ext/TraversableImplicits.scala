package scutil.ext

import scala.collection.generic.CanBuildFrom

import scutil.data._

object TraversableImplicits extends TraversableImplicits

trait TraversableImplicits {
	implicit def toTraversableExt[T,CC[T]<:Traversable[T]](delegate:CC[T])	= new TraversableExt[T,CC](delegate)
}

final class TraversableExt[T,CC[T]<:Traversable[T]](delegate:CC[T]) {
	/** get the only element of the collection or throw an Exception */
	def single:T = {
		var out:Option[T]	= None
		delegate foreach { it =>
			if (out.isDefined)	sys error "expected exactly one element, found multiple"
			out	= Some(it)
		}
		if (out.isEmpty)	sys error "expected exactly one element, found none"
		out.get
	}
	
	/** Some if the collections contains exactly one element, else None */
	def singleOption:Option[T]	= {
		var out:Option[T]	= None
		delegate foreach { it =>
			if (out.isDefined)	return None
			out	= Some(it)
		}
		out
	}
	
	// NOTE these don't terminate for infinite collections!
	
	/** pair elements of a collection with a function applied to an element */
	def zipBy[U](func:T=>U)(implicit cbf:CanBuildFrom[CC[T],(T,U),CC[(T,U)]]):CC[(T,U)]	= {
		val builder	= cbf()
		delegate foreach { it =>
			builder	+= ((it, func(it)))
		}
		builder.result
	}
	
	/** combine elements of two collections using a function */
	def zipWith[U,V](that:Traversable[U])(f:(T,U)=>V)(implicit cbf:CanBuildFrom[CC[T],V,CC[V]]):CC[V]	= {
		val builder	= cbf()
		val	xi	= delegate.toIterator
		val yi	= that.toIterator
		while (xi.hasNext && yi.hasNext) {
			builder	+= f(xi.next, yi.next)
		}
		builder.result
	}
	
	/** create a map from all elements with a given function to generate the keys */
	def mapBy[S](key:T=>S):Map[S,T]	=
			delegate map { it => (key(it), it) } toMap;
			
	// NOTE these should be generalized to other AFs, not just Option and Tried
	// NOTE supplying pure and flatMap of a Monad would work, too!
	
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:T=>Option[U], cbf:CanBuildFrom[CC[T],U,CC[U]]):Option[CC[U]]	=
			traverseOption(identity[U])
	
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U,V](func:U=>V)(implicit ev:T=>Option[U], cbf:CanBuildFrom[CC[T],V,CC[V]]):Option[CC[V]]	= {
		val builder	= cbf()
		delegate map ev foreach {
			case Some(x)	=> builder	+= func(x)
			case None		=> return None
		}
		Some(builder.result)
	}
			
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def sequenceTried[F,W](implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]]):Tried[F,CC[W]]	=
			traverseTried(identity[W])
		
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def traverseTried[F,W,V](func:W=>V)(implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],V,CC[V]]):Tried[F,CC[V]]	= {
		val builder	= cbf()
		delegate map ev foreach {
			case Win(x)		=> builder	+= func(x)
			case Fail(x)	=> return Fail(x)
		}
		Win(builder.result)
	}
}
