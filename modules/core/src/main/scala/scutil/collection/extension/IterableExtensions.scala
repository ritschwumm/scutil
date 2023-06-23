package scutil.collection.extension

import scala.collection.IterableOps
import scala.collection.BuildFrom
import scala.collection.generic.IsIterable
import scala.collection.mutable.Builder

import scutil.lang.*
import scutil.lang.tc.*

object IterableExtensions {
	// TODO dotty move these into IterableExt to support pseudo-iterables, too
	implicit final class SimpleIterableExt[T](peer:Iterable[T]) {
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

		/** create a set from all elements with a given function to generate the items */
		def setBy[U](func:T=>U):Set[U]	=
			peer.map(func).toSet

		/** create a map from all elements with a given function to generate the keys */
		def mapBy[S](key:T=>S):Map[S,T]	=
			mapToMap(it => (key(it), it))

		def mapToMap[U,V](func:T=>(U,V)):Map[U,V]	=
			(peer map func).toMap

		// NOTE this does not exist in cats
		def flattenOptionFirst[U](using ev: T <:< Option[U]):Option[U]	=
			collectFirstSome(ev)

		/**
		return the first Some find creates from elements of this collection
		resembles collectFirst, but uses Function1[_,Option[_]] instead of a PartialFunction[_,_]
		*/
		// TODO cats mapFilterFirst would be better
		def collectFirstSome[U](find:T=>Option[U]):Option[U]	= {
			val iter	= peer.iterator
			while (iter.hasNext) {
				val opt	= find(iter.next())
				if (opt.isDefined)	return opt
			}
			None
		}

		def scanLeftNes[U](z: U)(op: (U, T) => U):Nes[U]	= {
			val seq = peer.scanLeft(z)(op).to(Seq)
			Nes.unsafeFromSeq(seq)
		}

		def scanRightNes[U](z: U)(op: (T, U) => U):Nes[U]	= {
			val seq = peer.scanRight(z)(op).to(Seq)
			Nes.unsafeFromSeq(seq)
		}
	}

	implicit final class IterableExt[Repr,T,Self](peer:Repr)(using isIterable:IsIterable[Repr] { type A = T; type C = Self  }) {
		private val ops	= isIterable(peer)

		// NOTE these don't terminate for infinite collections!

		/** pair elements of a collection with a function applied to an element */
		def fproduct[That,U](func:T=>U)(using bf:BuildFrom[Repr,(T,U),That]):That	=
			bf.fromSpecific(peer)(
				ops.map(it => (it, func(it)))
			)

		def partitionEither[L,R,CL,CR](using ev:T <:< Either[L,R], bfl:BuildFrom[Repr,L,CL], bfr:BuildFrom[Repr,R,CR]):(CL, CR)	= {
			val (ls, rs)	= ops.partitionMap(ev)
			val cl	= bfl.fromSpecific(peer)(ls)
			val cr	= bfr.fromSpecific(peer)(rs)
			(cl, cr)
		}

		def partitionValidated[B,G,CB,CG](using ev:T <:< Validated[B,G], bfb:BuildFrom[Repr,B,CB], bfg:BuildFrom[Repr,G,CG]):(CB, CG)	= {
			val (bs, gs)	= ops.partitionMap(it => ev(it).toEither)
			val cb	= bfb.fromSpecific(peer)(bs)
			val cg	= bfg.fromSpecific(peer)(gs)
			(cb, cg)
		}

		def validateEither[L,R,CL,CR](using ev:T <:< Either[L,R], bfl:BuildFrom[Repr,L,CL], bfr:BuildFrom[Repr,R,CR]):Either[CL,CR]	= {
			val (ls, rs)	= ops.partitionMap(ev)
			if (ls.isEmpty)	Right(bfr.fromSpecific(peer)(rs))
			else			Left(bfl.fromSpecific(peer)(ls))
		}

		def validateValidated[B,G,CB,CG](using ev:T <:< Validated[B,G], bfb:BuildFrom[Repr,B,CB], bfg:BuildFrom[Repr,G,CG]):Validated[CB,CG]	= {
			val (bs, gs)	= ops.partitionMap(it => ev(it).toEither)
			if (bs.isEmpty)	Validated.valid(bfg.fromSpecific(peer)(gs))
			else			Validated.invalid(bfb.fromSpecific(peer)(bs))
		}

		// NOTE zipWith and map2 are different things for Iterables
		/** combine elements of two collections using a function */
		def zipWith[That,U,V](that:Iterable[U])(func:(T,U)=>V)(using bf:BuildFrom[Repr,V,That]):That	= {
			val builder	= bf.newBuilder(peer)
			val	xi	= ops.iterator
			val yi	= that.iterator
			while (xi.hasNext && yi.hasNext) {
				builder	+= func(xi.next(), yi.next())
			}
			builder.result()
		}

		def zipTail[That](using bf:BuildFrom[Repr,(T,T),That]):That	= {
			val builder	=  bf.newBuilder(peer)
			if (ops.nonEmpty) {
				val now		= ops.iterator
				val later	= ops.iterator
				later.next()
				while {
					builder += ((now.next(), later.next()))
					later.hasNext
				}
				do ()
			}
			builder.result()
		}

		/** like flatten, but avoiding the dubious Option=>Iterable implicit */
		def flattenOption[That,U](using bf:BuildFrom[Repr,U,That], ev: T <:< Option[U]):That	=
			mapFilter(ev)

		/** like flatMap, but avoiding the dubious Option=>Iterable implicit  */
		def mapFilter[That,U](func:T=>Option[U])(using bf:BuildFrom[Repr,U,That]):That	= {
			val builder	= bf.newBuilder(peer)
			ops foreach {
				func(_) foreach {
					builder	+= _
				}
			}
			builder.result()
		}

		/** insert a separator between elements */
		def intersperse[That,U>:T](separator: =>U)(using bf:BuildFrom[Repr,U,That]):That	= {
			val bld:Builder[U,That]	= bf.newBuilder(peer)
			if (ops.nonEmpty) {
				val iter	= ops.iterator
				var first	= true
				while (iter.hasNext) {
					if (first)	first = false
					else		bld	+= separator
					bld	+= iter.next()
				}
			}
			bld.result()
		}

		// NOTE these should be generalized to other AFs, not just Option and Tried
		// NOTE supplying pure and flatMap of a Monad would work, too!

		def sequenceOption[That,U](using bf:BuildFrom[Repr,U,That], ev:T <:< Option[U]):Option[That]	=
			traverseOption(ev)

		def traverseOption[That,U](func:T=>Option[U])(using bf:BuildFrom[Repr,U,That]):Option[That]	= {
			val builder	= bf.newBuilder(peer)
			val iter	= ops.iterator
			while (iter.hasNext) {
				func(iter.next()) match {
					case None		=> return None
					case Some(x)	=> builder	+= x
				}
			}
			Some(builder.result())
		}

		def sequenceEither[That,L,R](using bf:BuildFrom[Repr,R,That], ev:T <:< Either[L,R]):Either[L,That]	=
			traverseEither(ev)

		def traverseEither[That,L,R](func:T=>Either[L,R])(using bf:BuildFrom[Repr,R,That]):Either[L,That]	= {
			val builder	= bf.newBuilder(peer)
			val iter	= ops.iterator
			while (iter.hasNext) {
				func(iter.next()) match {
					case Left(x)	=> return Left(x)
					case Right(x)	=> builder	+= x
				}
			}
			Right(builder.result())
		}

		/** peer is traversable (in the haskell sense), Validated is an idiom. */
		def sequenceValidated[That,I,V](using bf:BuildFrom[Repr,V,That], cc:Semigroup[I], ev:T <:< Validated[I,V]):Validated[I,That]	=
			traverseValidated(ev)

		/** peer is traversable (in the haskell sense), Validated is an idiom. */
		def traverseValidated[That,I,V](func:T=>Validated[I,V])(using bf:BuildFrom[Repr,V,That], cc:Semigroup[I]):Validated[I,That]	= {
			var problems:Option[I]	= None
			val builder	= bf.newBuilder(peer)
			ops foreach { it =>
				func(it) match {
					case Validated.Valid(x)	=>
						if (problems.isEmpty) {
							builder	+= x
						}
					case Validated.Invalid(x)		=>
						problems	= problems match {
							case None		=> Some(x)
							case Some(p)	=> Some(cc.combine(p, x))
						}
				}
			}
			problems match {
				case Some(p)	=> Validated.invalid(p)
				case None		=> Validated.valid(builder.result())
			}
		}

		def sequenceState[That,S,U](using bf:BuildFrom[Repr,U,That], ev:T <:< State[S,U]):State[S,That]	=
			traverseState(ev)

		def traverseState[That,S,U](func:T=>State[S,U])(using bf:BuildFrom[Repr,U,That]):State[S,That]	=
			State { s =>
				var temp	= s
				val builder	= bf.newBuilder(peer)
				ops foreach { it =>
					val (next, part)	= func(it) run temp
					temp	= next
					builder += part
				}
				(temp, builder.result())
			}

		// TODO state support StateT
	}
}
