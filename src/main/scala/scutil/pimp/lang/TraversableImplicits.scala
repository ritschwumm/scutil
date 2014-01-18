package scutil.pimp

import scala.collection.generic.CanBuildFrom

import scutil.lang._

object TraversableImplicits extends TraversableImplicits

trait TraversableImplicits {
	implicit def toTraversableExt[T,CC[T]<:Traversable[T]](peer:CC[T])	= new TraversableExt[T,CC](peer)
}

final class TraversableExt[T,CC[T]<:Traversable[T]](peer:CC[T]) {
	/** get the only element of the collection or throw an Exception */
	def single:T = {
		var out:Option[T]	= None
		peer foreach { it =>
			if (out.isDefined)	sys error "expected exactly one element, found multiple"
			out	= Some(it)
		}
		if (out.isEmpty)	sys error "expected exactly one element, found none"
		out.get
	}
	
	/** Some if the collections contains exactly one element, else None */
	def singleOption:Option[T]	= {
		var out:Option[T]	= None
		peer foreach { it =>
			if (out.isDefined)	return None
			out	= Some(it)
		}
		out
	}
	
	// NOTE these don't terminate for infinite collections!
	
	/** pair elements of a collection with a function applied to an element */
	def zipBy[U](func:T=>U)(implicit cbf:CanBuildFrom[CC[T],(T,U),CC[(T,U)]]):CC[(T,U)]	= {
		val builder	= cbf()
		peer foreach { it =>
			builder	+= ((it, func(it)))
		}
		builder.result
	}
	
	/** combine elements of two collections using a function */
	def zipWith[U,V](that:Traversable[U])(f:(T,U)=>V)(implicit cbf:CanBuildFrom[CC[T],V,CC[V]]):CC[V]	= {
		val builder	= cbf()
		val	xi	= peer.toIterator
		val yi	= that.toIterator
		while (xi.hasNext && yi.hasNext) {
			builder	+= f(xi.next, yi.next)
		}
		builder.result
	}
	
	def splitEither[U,V](implicit ev:T=>Either[U,V], cbf1:CanBuildFrom[CC[T],U,CC[U]], cbf2:CanBuildFrom[CC[T],V,CC[V]]):(CC[U],CC[V])	= {
		val	builder1	= cbf1()
		val	builder2	= cbf2()
		peer map ev foreach {
			case Left(x)	=> builder1	+= x
			case Right(x)	=> builder2	+= x
		}
		(builder1.result, builder2.result)
	} 
	
	def splitTried[F,W](implicit ev:T=>Tried[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):(CC[F],CC[W])	= {
		val	builder1	= cbf1()
		val	builder2	= cbf2()
		peer map ev foreach {
			case Fail(x)	=> builder1	+= x
			case Win(x)		=> builder2	+= x
		}
		(builder1.result, builder2.result)
	} 
	
	/** create a map from all elements with a given function to generate the keys */
	def mapBy[S](key:T=>S):Map[S,T]	=
			(peer map { it => (key(it), it) }).toMap
			
	/** like flatten, but avoiding the dubious Option=>Iterable implicit */
	def collapse[U](implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],U,CC[U]]):CC[U]	= {
		val builder	= cbf()
		peer foreach {
			ev(_) foreach {
				builder	+= _
			}
		}
		builder.result
	}
	
	// NOTE these should be generalized to other AFs, not just Option and Tried
	// NOTE supplying pure and flatMap of a Monad would work, too!
	
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],U,CC[U]]):Option[CC[U]]	=
			traverseOption(identity[U])
	
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U,V](func:U=>V)(implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],V,CC[V]]):Option[CC[V]]	= {
		val builder	= cbf()
		peer map ev foreach {
			case Some(x)	=> builder	+= func(x)
			case None		=> return None
		}
		Some(builder.result)
	}
			
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def sequenceTried[F,W](implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]]):Tried[F,CC[W]]	=
			traverseTried(identity[W])
		
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def traverseTried[F,W,V](func:W=>V)(implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],V,CC[V]]):Tried[F,CC[V]]	= {
		val builder	= cbf()
		peer map ev foreach {
			case Win(x)		=> builder	+= func(x)
			case Fail(x)	=> return Fail(x)
		}
		Win(builder.result)
	}
	
	/** all Fails if there is at least one, else all Wins */
	def validateTried[F,W](implicit ev:T=>Tried[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):Tried[CC[F],CC[W]]	= {
		val (fails, wins)	= splitTried
		if (fails.isEmpty)	Win(wins)
		else				Fail(fails)
	}
	
	/** peer is traversable (in the haskell sense), Validated is an idiom. */
	def sequenceValidated[F,W](implicit ev:T=>Validated[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]]):Validated[F,CC[W]]	=
			traverseValidated(identity[W])
		
	/** peer is traversable (in the haskell sense), Validated is an idiom. */
	def traverseValidated[F,W,V](func:W=>V)(implicit ev:T=>Validated[F,W], cbf:CanBuildFrom[CC[T],V,CC[V]]):Validated[F,CC[V]]	= {
		// TODO ugly
		val mapped		= peer map ev
		val problems	= mapped flatMap { _.badProblems }
		Nes fromSeq problems.toVector match {
			case Some(es)	=> Bad(es)
			case None		=> 
				val builder	= cbf()
				mapped foreach {
					case Good(x)	=> builder	+= func(x)
					case Bad(_)		=>
				}
				Good(builder.result)
		}	
	}
}
