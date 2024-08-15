package scutil.collection.extension

import scala.collection.Factory
import scala.collection.BuildFrom
import scala.collection.generic.IsSeq
import scala.collection.mutable.Builder

import scutil.lang.*

object SeqExtensions {
	implicit final class SeqExt[Repr,T,Self](peer:Repr)(using isSeq:IsSeq[Repr] { type A = T; type C = Self }) {
		private val ops	= isSeq(peer)

		def toNesOption:Option[Nes[T]]	=
			Nes.fromSeq(ops.toSeq)

		def tailOption:Option[Self]	=
			if (ops.nonEmpty)	Some(ops.tail)
			else				None

		def initOption:Option[Self]	=
			if (ops.nonEmpty)	Some(ops.init)
			else				None

		/** get first item and rest of the Seq if possible */
		def extractHead:Option[(T,Self)]	=
			if (ops.nonEmpty)	Some((ops.head, ops.tail))
			else				None

		/** get last item and rest of the Seq if possible */
		def extractLast:Option[(T,Self)]	=
			if (ops.nonEmpty)	Some((ops.last, ops.init))
			else				None

		/** map the value for a single index */
		def updatedBy(index:Int, func:T=>T)(using factory:Factory[T,Self]):Option[Self]	=
			if (containsIndex(index))	Some(factory.fromSpecific(ops.updated(index, func(ops(index)))))
			else						None

		/** None when the index is outside our bounds */
		def updatedAt(index:Int, item:T)(using factory:Factory[T,Self]):Option[Self]	=
			if (containsIndex(index))	Some(factory.fromSpecific(ops.updated(index, item)))
			else						None

		/** insert an item at a given index if possible */
		def insertAt(gap:Int, item:T)(using factory:Factory[T,Self]):Option[Self]	=
			if (containsGap(gap))	Some(factory.fromSpecific(ops.patch(gap, Seq(item), 0)))
			else					None

		/** remove the item at a given index if possible */
		def removeAt(index:Int)(using factory:Factory[T,Self]):Option[Self]	=
			if (containsIndex(index))	Some(factory.fromSpecific(ops.patch(index, Seq.empty, 1)))
			else						None

		// aliases with low precendence

		def lastIndex:Int		= ops.size-1

		/** whether index is a gap between elements of this Seq */
		def containsGap(index:Int):Boolean	=
			index >= 0 && index <= ops.size

		/** whether index is an item in this Seq */
		def containsIndex(index:Int):Boolean	=
			index >= 0 && index < ops.size

		/*
		// NOTE this exists in monoid syntax already
		concatenate the Seq with itself n times
		def times(count:Int)(using factory:Factory[T,Repr]):Repr	=
			factory.fromSpecific(
				(0 until count).flatMap { _ => peer }
			)
		*/

		def indexOfOption(item:T):Option[Int]	=
			indexOption(ops indexOf item)

		def lastIndexOfOption(item:T):Option[Int]	=
			indexOption(ops lastIndexOf item)

		def indexWhereOption(pred:Predicate[T]):Option[Int]	=
			indexOption(ops indexWhere pred)

		def lastIndexWhereOption(pred:Predicate[T]):Option[Int]	=
			indexOption(ops lastIndexWhere pred)

		/** get value at an index or return a default value */
		def liftOrElse(index:Int, default:T):T	=
			if (containsIndex(index)) ops.apply(index) else default

		/** like collectFirst but searching from the end to the front */
		def collectLast[U](pf:PartialFunction[T,U]):Option[U]	=
			// behaves like peer.reverse collectFirst pf
			collectLastSome(pf.lift)

		def flattenOptionLast[U](using ev:T <:< Option[U]):Option[U]	=
			collectLastSome(ev)

		/** like collectLast but using a PFunction */
		// TODO cats mapFilterLast would be better
		def collectLastSome[U](find:T=>Option[U]):Option[U]	= {
			val iter	= ops.reverseIterator
			while (iter.hasNext) {
				val out	= find(iter.next())
				if (out.isDefined)	return out
			}
			None
		}

		/** get item at index and the Seq without that element if possible */
		def extractAt(index:Int)(using factory:Factory[T,Repr]):Option[(T,Repr)]	=
			lift(index).map { it =>
				(
					it,
					factory.fromSpecific(ops.patch(index, Seq.empty, 1))
				)
			}

		/** distinct with a custom equality check */
		def distinctWith(same:(T,T)=>Boolean)(using factory:Factory[T,Repr]):Repr =
			factory.fromSpecific(
				ops.foldLeft(Seq.empty[T]) { (retained:Seq[T], candidate:T) =>
					retained find { same(_,candidate) } match {
						case Some(_)	=> retained
						case None		=> candidate +: retained
					}
				}
				.reverse
			)

		/** triple every item with its previous and next item */
		def adjacents[That](using bf:BuildFrom[Repr, (Option[T],T,Option[T]), That]):That	= {
			val last	= ops.size-1
			val out	=
				ops.indices
				.map { idx =>
					(
						if (idx > 0)	Some(ops(idx-1)) else None,
						ops(idx),
						if (idx < last)	Some(ops(idx+1)) else None
					)
				}
			bf.fromSpecific(peer)(out)
		}

		/** optionally insert something between two items */
		def insertBetween[That,U](mod:T=>U, func:(T,T)=>Option[U])(using bf:BuildFrom[Repr,U,That]):That	=
			if (ops.size > 1) {
				val	out		= bf.newBuilder(peer)

				val iter	= ops.iterator
				var old		= iter.next()
				out	+= mod(old)

				while (iter.hasNext) {
					val cur	= iter.next()
					func(old, cur) foreach out.+=
					out	+= mod(cur)
					old	= cur
				}

				out.result()
			}
			else {
				bf.fromSpecific(peer)(ops.map(mod))
			}

		/** separators go to the Left, the rest goes into Right Seqs  */
		def splitWhere[That](separator:Predicate[T])(using factory:Factory[T,Repr], bf:BuildFrom[Repr,Either[T,Repr],That]):That = {
			val outer:Builder[Either[T,Repr],That]	= bf.newBuilder(peer)
			if (ops.nonEmpty) {
				val inner:Builder[T,Repr]	= factory.newBuilder
				val iter	= ops.iterator
				while (iter.hasNext) {
					val cur	= iter.next()
					if (separator(cur)) {
						outer	+= Right(inner.result())
						inner.clear()
						outer	+= Left(cur)
					}
					else {
						inner	+= cur
					}
				}
				outer	+= Right(inner.result())
			}
			outer.result()
		}

		/** equivalent elements go into own Seqs */
		def equivalentSpans[That](equivalent:(T,T)=>Boolean)(using factory:Factory[T,Repr], bf:BuildFrom[Repr,Repr,That]):That	= {
			val outer:Builder[Repr,That]	= bf.newBuilder(peer)
			//out += 1
			if (ops.nonEmpty) {
				val inner:Builder[T,Repr]	= factory.newBuilder

				val iter	= ops.iterator
				var old		= iter.next()
				inner	+= old

				while (iter.hasNext) {
					val cur	= iter.next()
					if (!equivalent(old, cur)) {
						outer	+= inner.result()
						inner.clear()
					}
					inner	+= cur
					old	= cur
				}

				outer	+= inner.result()
			}
			outer.result()
		}

		/** equivalentSpans on a single property */
		def equivalentSpansBy[That,U](extract:T=>U)(using factory:Factory[T,Repr], bf:BuildFrom[Repr,Repr,That]):That	=
			equivalentSpans { extract(_) == extract(_) }

		/** move an item from a given item index to an inter-item gap */
		def moveAt(fromIndex:Int, toGap:Int)(using factory:Factory[T,Repr]):Option[Repr]	=
			if (containsIndex(fromIndex) && containsGap(toGap)) {
				if (fromIndex < toGap-1) {
					// move right
					val bld:Builder[T,Repr]	= factory.newBuilder
					var i = 0
					while (i < ops.size) {
						val orig	=
							if		(i < fromIndex)	i
							else if	(i >= toGap)	i
							else if (i == toGap-1)	fromIndex
							else					i+1
						bld	+= ops(orig)
						i	+= 1
					}
					Some(bld.result())
				}
				else if (fromIndex > toGap) {
					// move left
					val bld:Builder[T,Repr]	= factory.newBuilder
					var i = 0
					while (i < ops.size) {
						val orig	=
							if		(i < toGap)		i
							else if	(i > fromIndex)	i
							else if (i == toGap)	fromIndex
							else					i-1
						bld	+= ops(orig)
						i	+= 1
					}
					Some(bld.result())
				}
				else None
			}
			else None

		/** move multiple items item from a given index to a inter-item gap of another index if possible */
		def moveManyAt(fromIndex:Int, count:Int, toGap:Int)(using factory:Factory[T,Repr]):Option[Repr]	=
			if (
				fromIndex	>= 0 && fromIndex+count	<= ops.size &&
				toGap		>= 0 && toGap			<= ops.size
			) {
				Some(
					if (fromIndex < toGap) {
						// move right
						val bld:Builder[T,Repr]	= factory.newBuilder
						val items	= ops.toSeq
						bld	++= items.slice(0,					fromIndex)
						bld	++= items.slice(fromIndex + count,	toGap)
						bld	++= items.slice(fromIndex,			fromIndex + count)
						bld	++= items.slice(toGap,				ops.size)
						bld.result()
					}
					else if (fromIndex > toGap) {
						// move left
						val bld:Builder[T,Repr]	= factory.newBuilder
						val items	= ops.toSeq
						bld	++= items.slice(0,					toGap)
						bld	++= items.slice(fromIndex,			fromIndex + count)
						bld	++= items.slice(toGap,				fromIndex)
						bld	++= items.slice(fromIndex + count,	ops.size)
						bld.result()
					}
					else peer
				)
			}
			else None

		def storeAt(index:Int)(using factory:Factory[T,Repr]):Option[Store[T,Repr]]	=
			lift(index).map { item =>
				Store[T,Repr](
					item,
					it => factory.fromSpecific(ops.updated(index, it))
				)
			}

		//------------------------------------------------------------------------------

		private def indexOption(index:Int):Option[Int]	=
			if (index != -1)	Some(index)
			else				None

		private def lift(index:Int):Option[T]	=
			if (containsIndex(index)) Some(ops.apply(index)) else None
	}
}
