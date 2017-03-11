package scutil.collection.pimp

import scala.collection.generic.CanBuildFrom

import scutil.lang._
import scutil.lang.tc._

object TraversableImplicits extends TraversableImplicits

trait TraversableImplicits {
	implicit def toTraversableExt[T,CC[T]<:Traversable[T]](peer:CC[T])	= new TraversableExt[T,CC](peer)
}

final class TraversableExt[T,CC[T]<:Traversable[T]](peer:CC[T]) {
	/** Some if the collection contains exactly one element, else None */
	def singleOption:Option[T]	= {
		var out:Option[T]	= None
		peer foreach { it =>
			if (out.isDefined)	return None
			out	= Some(it)
		}
		out
	}
	
	/** Fail(false) for zero elements, Win(x) for one element, Fail(true) for more */
	def singleTried:Tried[Boolean,T] = {
		var out:Option[T]	= None
		peer foreach { it =>
			if (out.isDefined)	return Fail(true)
			out	= Some(it)
		}
		out match {
			case Some(x)	=> Win(x)
			case None		=> Fail(false)
		}
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
	
	def partitionEither[U,V](implicit ev:T=>Either[U,V], cbf1:CanBuildFrom[CC[T],U,CC[U]], cbf2:CanBuildFrom[CC[T],V,CC[V]]):(CC[U],CC[V])	= {
		val	builder1	= cbf1()
		val	builder2	= cbf2()
		peer map ev foreach {
			case Left(x)	=> builder1	+= x
			case Right(x)	=> builder2	+= x
		}
		(builder1.result, builder2.result)
	}
	
	def partitionTried[F,W](implicit ev:T=>Tried[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):(CC[F],CC[W])	= {
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
	def collapse[U](implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],U,CC[U]]):CC[U]	=
			collapseMap(ev)
	
	/** like flatMap, but avoiding the dubious Option=>Iterable implicit */
	def collapseMap[U](func:PFunction[T,U])(implicit cbf:CanBuildFrom[CC[T],U,CC[U]]):CC[U]	= {
		val builder	= cbf()
		peer foreach {
			func(_) foreach {
				builder	+= _
			}
		}
		builder.result
	}
	
	// NOTE these should be generalized to other AFs, not just Option and Tried
	// NOTE supplying pure and flatMap of a Monad would work, too!
	
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:PFunction[T,U], cbf:CanBuildFrom[CC[T],U,CC[U]]):Option[CC[U]]	=
			traverseOption(ev)
	
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U](func:PFunction[T,U])(implicit cbf:CanBuildFrom[CC[T],U,CC[U]]):Option[CC[U]]	= {
		val builder	= cbf()
		peer map func foreach {
			case Some(x)	=> builder	+= x
			case None		=> return None
		}
		Some(builder.result)
	}
			
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def sequenceTried[F,W](implicit ev:T=>Tried[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]]):Tried[F,CC[W]]	=
			traverseTried(ev)
		
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def traverseTried[F,W](func:T=>Tried[F,W])(implicit cbf:CanBuildFrom[CC[T],W,CC[W]]):Tried[F,CC[W]]	= {
		val builder	= cbf()
		peer map func foreach {
			case Win(x)		=> builder	+= x
			case Fail(x)	=> return Fail(x)
		}
		Win(builder.result)
	}
	
	/** all Fails if there is at least one, else all Wins */
	def validateTried[F,W](implicit ev:T=>Tried[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):Tried[CC[F],CC[W]]	= {
		val (fails, wins)	= partitionTried
		if (fails.isEmpty)	Win(wins)
		else				Fail(fails)
	}
	
	/** peer is traversable (in the haskell sense), Validated is an idiom. */
	def sequenceValidated[F,W](implicit ev:T=>Validated[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]], cc:Semigroup[F]):Validated[F,CC[W]]	=
			traverseValidated(ev)
		
	/** peer is traversable (in the haskell sense), Validated is an idiom. */
	def traverseValidated[F,W](func:T=>Validated[F,W])(implicit cbf:CanBuildFrom[CC[T],W,CC[W]], cc:Semigroup[F]):Validated[F,CC[W]]	= {
		var problems:Option[F]	= None
		val builder				= cbf()
		peer foreach { it =>
			func(it) match {
				case Good(x)	=> if (problems.isEmpty)	builder	+= x
				case Bad(x)		=> problems map { p => cc concat (p,x) } orElse Some(x)
			}
		}
		problems map Bad.apply getOrElse Good(builder.result)
	}
	
	def toISeq:ISeq[T]	=
			peer match {
				case x:ISeq[T]	=> x
				case x			=> x.toVector
			}
}
