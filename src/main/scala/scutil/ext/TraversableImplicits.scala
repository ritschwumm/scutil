package scutil.ext

import scala.collection.generic.CanBuildFrom

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
	
	// NOTE these don't terminate for inifinite collections!
	
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
	def mapBy[S](index:T=>S):Map[S,T]	=
			delegate map { it => (index(it), it) } toMap;
			
	// NOTE this should be generalized to other AFs, not just Option
	// NOTE supplying pure and flatMap of a Monad wold work, too!
	
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:T=>Option[U], cbf:CanBuildFrom[CC[T],U,CC[U]]):Option[CC[U]]	=
			traverseOption(identity[U])
	
	/** Delegate is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U,V](func:U=>V)(implicit ev:T=>Option[U], cbf:CanBuildFrom[CC[T],V,CC[V]]):Option[CC[V]]	=
			if (delegate forall { _.isDefined }) {
				val builder	= cbf()
				delegate foreach { it =>
					builder	+= func(it.get)
				}
				Some(builder.result)
			}	
			else {
				None
			}
}
