package scutil.collection.pimp

import scala.collection.Factory

import scutil.lang._
import scutil.lang.tc._

object IterableImplicits extends IterableImplicits

trait IterableImplicits {
	implicit final class IterableExt[T,CC[T]<:Iterable[T]](peer:CC[T]) {
		/** Some if the collection contains exactly one element, else None */
		def singleOption:Option[T]	= {
			val iter	= peer.iterator
			if (!iter.hasNext)	return None
			val out		= iter.next()
			if (iter.hasNext)	return None
			Some(out)
		}

		/** Left(false) for zero elements, Right(x) for one element, Left(true) for more */
		def singleEither:Either[Boolean,T] = {
			val iter	= peer.iterator
			if (!iter.hasNext)	return Left(false)
			val out		= iter.next()
			if (iter.hasNext)	return Left(true)
			Right(out)
		}

		// NOTE these don't terminate for infinite collections!

		/*
		// NOTE this would work, too
		def zipBy[U](func:T=>U)(implicit factory:Factory[(T,U),CC[(T,U)]]):CC[(T,U)]	=
			factory fromSpecific (
				peer map { it => (it, func(it)) }
			)
		*/

		/** pair elements of a collection with a function applied to an element */
		def zipBy[U](func:T=>U)(implicit factory:Factory[(T,U),CC[(T,U)]]):CC[(T,U)]	= {
			val builder	= factory.newBuilder
			peer foreach { it =>
				builder	+= ((it, func(it)))
			}
			builder.result
		}

		/** combine elements of two collections using a function */
		def zipWith[U,V](that:Iterable[U])(f:(T,U)=>V)(implicit factory:Factory[V,CC[V]]):CC[V]	= {
			val builder	= factory.newBuilder
			val	xi	= peer.iterator
			val yi	= that.iterator
			while (xi.hasNext && yi.hasNext) {
				builder	+= f(xi.next, yi.next)
			}
			builder.result
		}

		def partitionEither[U,V](implicit ev:T=>Either[U,V], factory1:Factory[U,CC[U]], factory2:Factory[V,CC[V]]):(CC[U],CC[V])	= {
			val	builder1	= factory1.newBuilder
			val	builder2	= factory2.newBuilder
			peer map ev foreach {
				case Left(x)	=> builder1	+= x
				case Right(x)	=> builder2	+= x
			}
			(builder1.result, builder2.result)
		}

		def partitionValidated[U,V](implicit ev:T=>Validated[U,V], factory1:Factory[U,CC[U]], factory2:Factory[V,CC[V]]):(CC[U],CC[V])	= {
			val	builder1	= factory1.newBuilder
			val	builder2	= factory2.newBuilder
			peer map ev foreach {
				case Bad(x)		=> builder1	+= x
				case Good(x)	=> builder2	+= x
			}
			(builder1.result, builder2.result)
		}

		/** create a set from all elements with a given function to generate the items */
		def setBy[U](func:T=>U):Set[U]	=
				peer.map(func).toSet

		/** create a map from all elements with a given function to generate the keys */
		def mapBy[S](key:T=>S):Map[S,T]	=
				mapToMap(it => (key(it), it))

		def mapToMap[U,V](func:T=>(U,V)):Map[U,V]	=
				(peer map func).toMap

		/** like flatten, but avoiding the dubious Option=>Iterable implicit */
		def collapse[U](implicit ev:PFunction[T,U], factory:Factory[U,CC[U]]):CC[U]	=
				collapseMap(ev)

		/** like flatMap, but avoiding the dubious Option=>Iterable implicit */
		def collapseMap[U](func:PFunction[T,U])(implicit factory:Factory[U,CC[U]]):CC[U]	= {
			val builder	= factory.newBuilder
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
			val iter	= peer.iterator
			while (iter.hasNext) {
				val opt	= find(iter.next())
				if (opt.isDefined)	return opt
			}
			None
		}

		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def maxOption(implicit ev:Ordering[T]):Option[T]	=
				if (peer.nonEmpty)	Some(peer.max)
				else				None

		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def minOption(implicit ev:Ordering[T]):Option[T]	=
				if (peer.nonEmpty)	Some(peer.min)
				else				None

		// NOTE these should be generalized to other AFs, not just Option and Tried
		// NOTE supplying pure and flatMap of a Monad would work, too!

		/** peer is traversable (in the haskell sense), Option is an idiom. */
		def sequenceOption[U](implicit ev:PFunction[T,U], factory:Factory[U,CC[U]]):Option[CC[U]]	=
				traverseOption(ev)

		/** peer is traversable (in the haskell sense), Option is an idiom. */
		def traverseOption[U](func:PFunction[T,U])(implicit factory:Factory[U,CC[U]]):Option[CC[U]]	= {
			val builder	= factory.newBuilder
			val iter	= peer.iterator
			while (iter.hasNext) {
				func(iter.next()) match {
					case None		=> return None
					case Some(x)	=> builder	+= x
				}
			}
			Some(builder.result)
		}

		/** peer is traversable (in the haskell sense), Either is an idiom. */
		def sequenceEither[F,W](implicit ev:T=>Either[F,W], factory:Factory[W,CC[W]]):Either[F,CC[W]]	=
				traverseEither(ev)

		/** peer is traversable (in the haskell sense), Either is an idiom. */
		def traverseEither[F,W](func:T=>Either[F,W])(implicit factory:Factory[W,CC[W]]):Either[F,CC[W]]	= {
			val builder	= factory.newBuilder
			val iter	= peer.iterator
			while (iter.hasNext) {
				func(iter.next()) match {
					case Left(x)	=> return Left(x)
					case Right(x)	=> builder	+= x
				}
			}
			Right(builder.result)
		}

		/** all Lefts if there is at least one, else all Rights */
		def validateEither[F,W](implicit ev:T=>Either[F,W], factory1:Factory[F,CC[F]], factory2:Factory[W,CC[W]]):Either[CC[F],CC[W]]	= {
			val (lefts, rights)	= partitionEither
			if (lefts.isEmpty)	Right(rights)
			else				Left(lefts)
		}

		/** peer is traversable (in the haskell sense), Validated is an idiom. */
		def sequenceValidated[F,W](implicit ev:T=>Validated[F,W], factory:Factory[W,CC[W]], cc:Semigroup[F]):Validated[F,CC[W]]	=
				traverseValidated(ev)

		/** peer is traversable (in the haskell sense), Validated is an idiom. */
		def traverseValidated[F,W](func:T=>Validated[F,W])(implicit factory:Factory[W,CC[W]], cc:Semigroup[F]):Validated[F,CC[W]]	= {
			var problems:Option[F]	= None
			val builder	= factory.newBuilder
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

		/** all Lefts if there is at least one, else all Rights */
		def validateValidated[F,W](implicit ev:T=>Validated[F,W], factory1:Factory[F,CC[F]], factory2:Factory[W,CC[W]]):Validated[CC[F],CC[W]]	= {
			val (bads, goods)	= partitionValidated
			if (bads.isEmpty)	Good(goods)
			else				Bad(bads)
		}

		def sequenceState[S,U](implicit ev:T=>State[S,U], factory:Factory[U,CC[U]]):State[S,CC[U]]	=
				traverseState(ev)

		def traverseState[S,U](func:T=>State[S,U])(implicit factory:Factory[U,CC[U]]):State[S,CC[U]]	=
				State { s =>
					var temp	= s
					val builder	= factory.newBuilder
					peer foreach { it =>
						val (next, part)	= func(it) run temp
						temp	= next
						builder += part
					}
					(temp, builder.result)
				}

		// TODO state support StateT

		@deprecated("use toSeq", "0.162.0")
		def toISeq:ISeq[T]	=
				toSeq

		// TODO doesn't seem to work on _*
		def toSeq:Seq[T]	=
				peer match {
					case x:Seq[T]	=> x
					case x			=> x.toVector
				}
	}
}
