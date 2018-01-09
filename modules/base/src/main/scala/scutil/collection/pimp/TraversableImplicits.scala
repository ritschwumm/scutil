package scutil.collection.pimp

import scala.collection.generic.CanBuildFrom

import scutil.lang._
import scutil.lang.tc._

object TraversableImplicits extends TraversableImplicits

trait TraversableImplicits {
	implicit final class TraversableExt[T,CC[T]<:Traversable[T]](peer:CC[T]) {
		/** Some if the collection contains exactly one element, else None */
		def singleOption:Option[T]	= {
			var out:Option[T]	= None
			peer foreach { it =>
				if (out.isDefined)	return None
				out	= Some(it)
			}
			out
		}
		
		/** Left(false) for zero elements, Right(x) for one element, Left(true) for more */
		def singleEither:Either[Boolean,T] = {
			var out:Option[T]	= None
			peer foreach { it =>
				if (out.isDefined)	return Left(true)
				out	= Some(it)
			}
			out match {
				case Some(x)	=> Right(x)
				case None		=> Left(false)
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
		
		def partitionValidated[U,V](implicit ev:T=>Validated[U,V], cbf1:CanBuildFrom[CC[T],U,CC[U]], cbf2:CanBuildFrom[CC[T],V,CC[V]]):(CC[U],CC[V])	= {
			val	builder1	= cbf1()
			val	builder2	= cbf2()
			peer map ev foreach {
				case Bad(x)		=> builder1	+= x
				case Good(x)	=> builder2	+= x
			}
			(builder1.result, builder2.result)
		}
		
		/** create a map from all elements with a given function to generate the keys */
		def mapBy[S](key:T=>S):Map[S,T]	=
				mapToMap(it => (key(it), it))
			
		def mapToMap[U,V](func:T=>(U,V)):Map[U,V]	=
				(peer map func).toMap
				
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
		
		/** like flatten, but avoiding the dubious Option=>Iterable implicit */
		def collapseFirst[U](implicit ev:PFunction[T,U]):Option[U]	=
				collapseMapFirst(ev)
			
		/**
		return the first Some find creates from elements of this collection
		resembles collectFirst, but uses Function1[_,Option[_]] instead of a PartialFunction[_,_]
		*/
		def collapseMapFirst[U](find:PFunction[T,U]):Option[U]	= {
			peer foreach { it =>
				val	out	= find(it)
				if (out.isDefined)	return out
			}
			None
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
				
		/** peer is traversable (in the haskell sense), Either is an idiom. */
		def sequenceEither[F,W](implicit ev:T=>Either[F,W], cbf:CanBuildFrom[CC[T],W,CC[W]]):Either[F,CC[W]]	=
				traverseEither(ev)
			
		/** peer is traversable (in the haskell sense), Either is an idiom. */
		def traverseEither[F,W](func:T=>Either[F,W])(implicit cbf:CanBuildFrom[CC[T],W,CC[W]]):Either[F,CC[W]]	= {
			val builder	= cbf()
			peer map func foreach {
				case Left(x)		=> return Left(x)
				case Right(x)		=> builder	+= x
			}
			Right(builder.result)
		}
		
		/** all Lefts if there is at least one, else all Rights */
		def validateEither[F,W](implicit ev:T=>Either[F,W], cbf1:CanBuildFrom[CC[T],F,CC[F]], cbf2:CanBuildFrom[CC[T],W,CC[W]]):Either[CC[F],CC[W]]	= {
			val (lefts, rights)	= partitionEither
			if (lefts.isEmpty)	Right(rights)
			else				Left(lefts)
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
					case Good(x)	=>
						if (problems.isEmpty) {
							builder	+= x
						}
					case Bad(x)		=>
						problems	= problems match {
							case None		=> Some(x)
							case Some(p)	=> Some(cc concat (p, x))
						}
				}
			}
			problems match {
				case Some(p)	=> Bad(p)
				case None		=> Good(builder.result)
			}
		}
		
		def sequenceState[S,U](implicit ev:T=>State[S,U], cbf:CanBuildFrom[CC[T],U,CC[U]]):State[S,CC[U]]	=
				traverseState(ev)
			
		def traverseState[S,U](func:T=>State[S,U])(implicit cbf:CanBuildFrom[CC[T],U,CC[U]]):State[S,CC[U]]	= {
			State { s =>
				var temp	= s
				val builder	= cbf()
				peer foreach { it =>
					val (next, part)	= func(it) run temp
					temp	= next
					builder += part
				}
				(temp, builder.result)
			}
		}
		
		// TODO state support StateT
		
		// TODO doesn't seem to work on _*
		def toISeq:ISeq[T]	=
				peer match {
					case x:ISeq[T]	=> x
					case x			=> x.toVector
				}
	}
}
