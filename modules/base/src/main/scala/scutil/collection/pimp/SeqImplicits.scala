package scutil.collection.pimp

import scala.collection.Factory
import scala.collection.SeqOps

import scutil.lang._

object SeqImplicits extends SeqImplicits

trait SeqImplicits {
	implicit final class SeqExt[CC[T] <: Seq[T],T](peer:CC[T]) {
		// aliases with low precendence

		def lastIndex:Int		= peer.size-1

		/** whether index is a gap between elements of this Seq */
		def containsGap(index:Int):Boolean	=
			index >= 0 && index <= peer.size

		/** whether index is an item in this Seq */
		def containsIndex(index:Int):Boolean	=
			index >= 0 && index < peer.size

		/** concatenate the Seq with itself n times */
		def times(count:Int)(implicit factory:Factory[T,CC[T]]):CC[T]	=
			(0 until count).flatMap { _ => peer }.to(factory)

		def indexOfOption(item:T):Option[Int]	=
			indexOption(peer indexOf item)

		def lastIndexOfOption(item:T):Option[Int]	=
			indexOption(peer lastIndexOf item)

		def indexWhereOption(pred:Predicate[T]):Option[Int]	=
			indexOption(peer indexWhere pred)

		def lastIndexWhereOption(pred:Predicate[T]):Option[Int]	=
			indexOption(peer lastIndexWhere pred)

		/** get value at an index or return a default value */
		def liftOrElse(index:Int, default:T):T	=
			peer lift index getOrElse default

		/** like collectFirst but searching from the end to the front */
		def collectLast[U](pf:PartialFunction[T,U]):Option[U]	=
			// behaves like peer.reverse collectFirst pf
			collapseLast(pf.lift)

		def collapseLast[U](implicit ev:PFunction[T,U]):Option[U]	=
			collapseMapLast(ev)

		// NOTE this is in IterableImplicits
		// def collapseMapFirst[U](find:PFunction[T,U]):Option[U]

		/** like collectLast but using a PFunction */
		def collapseMapLast[U](find:PFunction[T,U]):Option[U]	= {
			val iter	= peer.reverseIterator
			while (iter.hasNext) {
				val out	= find(iter.next())
				if (out.isDefined)	return out
			}
			None
		}

		/**
		 * group values by keys, both from a function.
		 * functionally this is the same as the builtin groupMap,
		 * but might trade some calculation for object allocations
		 */
		def groupMapPaired[K,V](func:T=>(K,V))(implicit factory:Factory[V,CC[V]]):Map[K,CC[V]]	=
			// TODO generify without factory
			peer
			.map		(func)
			.groupBy	{ _._1 }
			.map { case (k, kvs) =>
				(k, kvs map { _._2 } to factory)
			}

		/** get item at index and the Seq without that element if possible */
		def extractAt(index:Int)(implicit factory:Factory[T,CC[T]]):Option[(T,CC[T])]	=
			// TODO generify without factory - but lift is not in SeqOps
			peer lift index map { it =>
				(it, peer.patch (index, Seq.empty, 1).to(factory))
			}

		/** distinct with a custom equality check */
		def distinctWith(same:(T,T)=>Boolean)(implicit factory:Factory[T,CC[T]]):CC[T] =
			((peer foldLeft Seq.empty[T]) { (retained:Seq[T], candidate:T) =>
				retained find { same(_,candidate) } match {
					case Some(_)	=> retained
					case None		=> candidate +: retained
				}
			}).reverse.to(factory)

		/** triple every item with its previous and next item */
		def adjacents(implicit factory:Factory[(Option[T],T,Option[T]),CC[(Option[T],T,Option[T])]]):CC[(Option[T],T,Option[T])]	= {
			/*
			val somes	= peer map Some.apply
			val prevs	= None +: (somes dropRight 1)
			val nexts	= (somes drop 1) :+ None
			prevs zip peer zip nexts map assoc.unarrow3
			*/
			// TODO generify without factory - but we have to know drop, and that peer is a CC[U], too
			val last	= peer.size-1
			(0 until peer.size)
			.map { idx =>
				(
					if (idx > 0)	Some(peer(idx-1)) else None,
					peer(idx),
					if (idx < last)	Some(peer(idx+1)) else None
				)
			}
			.to(factory)
		}

		/** optionally insert something between two items */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def insertBetween[U](mod:T=>U, func:(T,T)=>Option[U])(implicit factory:Factory[U,CC[U]]):CC[U]	=
			if (peer.size < 2)	peer map mod to factory
			else {
				val	out	= factory.newBuilder
				peer zip peer.tail foreach { case (now,later) =>
					out	+= mod(now)
					func(now,later) foreach out.+=
				}
				out	+= mod(peer.last)
				out.result()
			}

		/** separators go to the Left, the rest goes into Right Seqs  */
		def splitWhere(separator:Predicate[T]):Seq[Either[T,Seq[T]]] =
			// TODO generify to CC
			if (peer.nonEmpty) {
				val indices = peer.zipWithIndex collect { case (t,i) if (separator(t)) => i }
				(-1 +: indices) zip (indices :+ peer.size) flatMap { case (a,b) =>
					Vector(Right(peer.slice(a+1, b))) ++
					(peer lift b map Left.apply).toList
				}
			}
			else Vector.empty

		/** equivalent elements go into own Seqs */
		def equivalentSpans(equivalent:(T,T)=>Boolean):Seq[Seq[T]]	= {
			// TODO generify to CC
			@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
			def impl(in:Seq[T]):Seq[Seq[T]]	= {
				val (a,b)	= in span { equivalent(in.head, _) }
				if (b.nonEmpty)	a +: impl(b)
				else			Vector(a)
			}
			if (peer.nonEmpty)	impl(peer)
			else				Vector.empty
		}

		/** equivalentSpans on a single property */
		def equivalentSpansBy[U](extract:T=>U):Seq[Seq[T]]	=
			equivalentSpans { extract(_) == extract(_) }

		/** move an item from a given item index to an inter-item gap */
		def moveAt(fromIndex:Int, toGap:Int)(implicit factory:Factory[T,CC[T]]):Option[CC[T]]	=
			// TODO generify without factory - but calling patch a second time is not possible on SeqOps
			if (containsIndex(fromIndex) && containsGap(toGap)) {
				if (fromIndex < toGap-1) {
					Some(
						peer
						.patch(toGap,		Seq(peer(fromIndex)),	0)
						.patch(fromIndex,	Seq.empty,				1)
						.to(factory)
					)
				}
				else if (fromIndex > toGap) {
					Some(
						peer
						.patch(fromIndex,	Seq.empty,				1)
						.patch(toGap,		Seq(peer(fromIndex)),	0)
						.to(factory)
					)
				}
				else None
			}
			else None

		/** move multiple items item from a given index to a inter-item gap of another index if possible */
		def moveManyAt(fromIndex:Int, count:Int, toGap:Int)(implicit factory:Factory[T,CC[T]]):Option[CC[T]]	=
			// TODO generify without factory - but calling patch a second time is not possible on SeqOps
			if (
				fromIndex	>= 0 && fromIndex+count	<= peer.size &&
				toGap		>= 0 && toGap			<= peer.size
			) {
				Some(
					(
						if (fromIndex < toGap) {
							// move right
							(peer.slice(0,						fromIndex))				++
							(peer.slice(fromIndex	+ count,	toGap))					++
							(peer.slice(fromIndex,				fromIndex	+ count))	++
							(peer.slice(toGap,					peer.size))
						}
						else if (fromIndex > toGap) {
							// move left
							(peer.slice(0,						toGap))				++
							(peer.slice(fromIndex,				fromIndex + count))	++
							(peer.slice(toGap,					fromIndex))			++
							(peer.slice(fromIndex	+ count,	peer.size))
						}
						else peer
					)
					.to(factory)

				)
			}
			else None

		def storeAt(index:Int)(implicit factory:Factory[T,CC[T]]):Option[Store[CC[T],T]]	=
			// TODO generify without factory - but lift is not in SeqOps
			peer lift index map { item =>
				Store[CC[T],T](
					item,
					peer.updated (index, _).to(factory)
				)
			}

		def toNesOption:Option[Nes[T]]	=
			Nes fromSeq peer

		private def indexOption(index:Int):Option[Int]	=
			if (index != -1)	Some(index)
			else				None
	}

	implicit final class SeqOpsExt[CC[_],T](peer:SeqOps[T,CC,CC[T]]) {
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def tailOption:Option[CC[T]]	=
			if (peer.nonEmpty)	Some(peer.tail)
			else				None

		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def initOption:Option[CC[T]]	=
			if (peer.nonEmpty)	Some(peer.init)
			else				None

		/** get first item and rest of the Seq if possible */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def extractHead:Option[(T,CC[T])]	=
			if (peer.nonEmpty)	Some((peer.head, peer.tail))
			else				None

		/** get last item and rest of the Seq if possible */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def extractLast:Option[(T,CC[T])]	=
			if (peer.nonEmpty)	Some((peer.last, peer.init))
			else				None

		/** map the value for a single index */
		def updatedBy(index:Int, func:Endo[T]):Option[CC[T]]	=
			if (containsIndex1(index))	Some(peer.updated(index, func(peer(index))))
			else						None

		/** None when the index is outside our bounds */
		def updatedAt(index:Int, item:T):Option[CC[T]]	=
			if (containsIndex1(index))	Some(peer.updated(index, item))
			else						None

		/** insert an item at a given index if possible */
		def insertAt(gap:Int, item:T):Option[CC[T]]	=
			if (containsGap1(gap))	Some(peer.patch(gap, Seq(item), 0))
			else 					None

		/** remove the item at a given index if possible */
		def removeAt(index:Int):Option[CC[T]]	=
			if (containsIndex1(index))	Some(peer.patch(index, Seq.empty, 1))
			else						None

		// TODO generify this exists in Seq implicits
		private def containsIndex1(index:Int):Boolean	=
			index >= 0 && index < peer.size

		// TODO generify this exists in Seq implicits
		private def containsGap1(index:Int):Boolean	=
			index >= 0 && index <= peer.size
	}

	implicit final class SeqWithOpsExt[CC[T] <: Seq[T],T](peer:SeqOps[T,CC,CC[T]]) {
		def withReverse(func:Endo[CC[T]])(implicit factory:Factory[T,CC[T]]):CC[T]	=
			// TODO generify without factory - but the reverse returns the wrong type
			func(peer.reverse).reverse.to(factory)

		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def zipTail(implicit factory:Factory[(T,T),CC[(T,T)]]):CC[(T,T)]	=
			(	if (peer.nonEmpty)	peer zip peer.tail
				else				factory.newBuilder.result()
			)
	}
}
