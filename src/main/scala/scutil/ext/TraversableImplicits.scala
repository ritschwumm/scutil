package scutil.ext

import scala.collection.generic.CanBuildFrom

import scutil.lang._
import scutil.tried._

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
	
	def cozipEither[U,V](implicit ev:T=>Either[U,V], cbf1:CanBuildFrom[CC[T],U,CC[U]], cbf2:CanBuildFrom[CC[T],V,CC[V]]):(CC[U],CC[V])	= {
		val	builder1	= cbf1()
		val	builder2	= cbf2()
		delegate map ev foreach {
			case Left(x)	=> builder1	+= x
			case Right(x)	=> builder2	+= x
		}
		(builder1.result, builder2.result)
	} 
	
	def cozipTried[F,W](implicit ev:T=>Tried[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):(CC[F],CC[W])	= {
		val	builder1	= cbf1()
		val	builder2	= cbf2()
		delegate map ev foreach {
			case Fail(x)	=> builder1	+= x
			case Win(x)		=> builder2	+= x
		}
		(builder1.result, builder2.result)
	} 
	
	/** create a map from all elements with a given function to generate the keys */
	def mapBy[S](key:T=>S):Map[S,T]	=
			(delegate map { it => (key(it), it) }).toMap
			
	// NOTE these should be generalized to other AFs, not just Option and Tried
	// NOTE supplying pure and flatMap of a Monad would work, too!
	
	/** delegate is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],U,CC[U]]):Option[CC[U]]	=
			traverseOption(identity[U])
	
	/** delegate is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U,V](func:U=>V)(implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],V,CC[V]]):Option[CC[V]]	= {
		val builder	= cbf()
		delegate map ev foreach {
			case Some(x)	=> builder	+= func(x)
			case None		=> return None
		}
		Some(builder.result)
	}
			
	/** delegate is traversable (in the haskell sense), Tried is an idiom. */
	def sequenceTried[F,W](implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]]):Tried[F,CC[W]]	=
			traverseTried(identity[W])
		
	/** delegate is traversable (in the haskell sense), Option is an idiom. */
	def traverseTried[F,W,V](func:W=>V)(implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],V,CC[V]]):Tried[F,CC[V]]	= {
		val builder	= cbf()
		delegate map ev foreach {
			case Win(x)		=> builder	+= func(x)
			case Fail(x)	=> return Fail(x)
		}
		Win(builder.result)
	}
	
	/** all Fails if there is at least one, else all Wins */
	def validateTried[F,W](implicit ev:T=>Tried[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):Tried[CC[F],CC[W]]	= {
		val (fails,wins)	= cozipTried
		if (fails.isEmpty)	Win(wins)
		else				Fail(fails)
	}
}
