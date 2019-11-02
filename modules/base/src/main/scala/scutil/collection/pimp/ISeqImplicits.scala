package scutil.collection.pimp

import scala.collection.immutable

import scutil.lang._

object ISeqImplicits extends ISeqImplicits

trait ISeqImplicits {
	implicit final class ISeqExt[T](peer:ISeq[T]) {
		// aliases with low precendence

		def prepend(it:T):ISeq[T]	= it +: peer
		def append(it:T):ISeq[T]	= peer :+ it

		def prependAll(it:Iterable[T]):ISeq[T]	= it ++: peer
		def appendAll(it:Iterable[T]):ISeq[T]	= peer ++ it

		def lastIndex:Int		= peer.size-1

		def indices:ISeq[Int]	= 0 until peer.size

		/** whether index is a gap between elements of this ISeq */
		def containsGap(index:Int):Boolean	=
				index >= 0 && index <= peer.size

		/** whether index is an item in this ISeq */
		def containsIndex(index:Int):Boolean	=
				index >= 0 && index < peer.size

		/** concatenate the ISeq with itself n times */
		def times(count:Int):ISeq[T]	=
				(0 until count).toVector flatMap { _ => peer }

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

		/** like find but searching from the end to the front */
		def findLast(predicate:Predicate[T]):Option[T]	=  {
			// behaves like peer.reverse find predicate
			val iter	= peer.reverseIterator
			while (iter.hasNext) {
				val out	= iter.next()
				if (predicate(out))	return Some(out)
			}
			None
		}

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

		/** group values by keys, both from a function */
		// TODO 312 already exists there
		def groupMap[K,V](func:T=>(K,V)):Map[K,ISeq[V]]	=
				peer
				.map		(func)
				.groupBy	{ _._1 }
				.map { case (k, kvs) =>
					(k, kvs map { _._2 })
				}

		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def tailOption:Option[ISeq[T]]	=
				if (peer.nonEmpty)	Some(peer.tail)
				else				None

		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def initOption:Option[ISeq[T]]	=
				if (peer.nonEmpty)	Some(peer.init)
				else				None

		/** get item at index and the ISeq without that element if possible */
		def extractAt(index:Int):Option[(T,ISeq[T])]	=
				peer lift index map { it =>
					(it, peer patch (index, ISeq.empty, 1))
				}

		/** get first item and rest of the ISeq if possible */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def extractHead:Option[(T,ISeq[T])]	=
				if (peer.nonEmpty)	Some((peer.head, peer.tail))
				else				None

		/** get last item and rest of the ISeq if possible */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def extractLast:Option[(T,ISeq[T])]	=
				if (peer.nonEmpty)	Some((peer.last, peer.init))
				else				None

		def withReverse(func:Endo[ISeq[T]]):ISeq[T]	=
				func(peer.reverse).reverse

		/** distinct with a custom equality check */
		def distinctWith(same:(T,T)=>Boolean):ISeq[T] =
				((peer foldLeft ISeq.empty[T]) { (retained:ISeq[T], candidate:T) =>
					retained find { same(_,candidate) } match {
						case Some(_)	=> retained
						case None		=> candidate +: retained
					}
				}).reverse

		/** distinct on a single property */
		def distinctBy[U](extract:T=>U):ISeq[T] =
				distinctWith { extract(_) == extract(_) }

		/** pairwise neighbors */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def zipTail:ISeq[(T,T)]	=
				if (peer.nonEmpty)	peer zip peer.tail
				else				Vector.empty

		/** insert a separator between elements, before the first and after the last item */
		def surround[U>:T](separator: =>U):ISeq[U]	=
				if (peer.nonEmpty)	(peer flatMap { ISeq(separator, _) }) :+ separator
				else				peer

		/** insert a separator between elements */
		def intersperse[U>:T](separator: =>U):ISeq[U]	=
				if (peer.nonEmpty)	peer flatMap { ISeq(separator, _) } drop 1
				else				peer

		/** triple every item with its previous and next item */
		def adjacents:ISeq[(Option[T],T,Option[T])]	= {
			/*
			val somes	= peer map Some.apply
			val prevs	= None +: (somes dropRight 1)
			val nexts	= (somes drop 1) :+ None
			prevs zip peer zip nexts map assoc.unarrow3
			*/
			val last	= peer.size-1
			0 until peer.size map { idx =>
				(
					if (idx > 0)	Some(peer(idx-1)) else None,
					peer(idx),
					if (idx < last)	Some(peer(idx+1)) else None
				)
			}
		}

		/** optionally insert something between two items */
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		def insertBetween[U](mod:T=>U, func:(T,T)=>Option[U]):ISeq[U]	=
				if (peer.size < 2)	peer map mod
				else {
					val	out	= new immutable.VectorBuilder[U]
					peer zip peer.tail foreach { case (now,later) =>
						out	+= mod(now)
						func(now,later) foreach out.+=
					}
					out	+= mod(peer.last)
					out.result
				}

		/** separators go to the Left, the rest goes into Right ISeqs  */
		def splitWhere(separator:Predicate[T]):ISeq[Either[T,ISeq[T]]] =
				if (peer.nonEmpty) {
					val indices = peer.zipWithIndex collect { case (t,i) if (separator(t)) => i }
					(-1 +: indices) zip (indices :+ peer.size) flatMap { case (a,b) =>
						Vector(Right(peer slice (a+1,b))) ++
						(peer lift b map Left.apply).toList
					}
				}
				else Vector.empty

		/** equivalent elements go into own ISeqs */
		def equivalentSpans(equivalent:(T,T)=>Boolean):ISeq[ISeq[T]]	= {
			@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
			def impl(in:ISeq[T]):ISeq[ISeq[T]]	= {
				val (a,b)	= in span { equivalent(in.head, _) }
				if (b.nonEmpty)	a +: impl(b)
				else			Vector(a)
			}
			if (peer.nonEmpty)	impl(peer)
			else				Vector.empty
		}

		/** equivalentSpans on a single property */
		def equivalentSpansBy[U](extract:T=>U):ISeq[ISeq[T]]	=
				equivalentSpans { extract(_) == extract(_) }

		/** map the value for a single index */
		def updatedBy(index:Int, func:Endo[T]):Option[ISeq[T]]	=
				if (containsIndex(index))	Some(peer updated (index, func(peer(index))))
				else						None

		/** None when the index is outside our bounds */
		def updatedAt(index:Int, item:T):Option[ISeq[T]]	=
				if (containsIndex(index))	Some(peer updated (index, item))
				else						None

		/** insert an item at a given index if possible */
		def insertAt(gap:Int, item:T):Option[ISeq[T]]	=
				if (containsGap(gap))	Some(peer patch (gap, ISeq(item), 0))
				else 					None

		/** remove the item at a given index if possible */
		def removeAt(index:Int):Option[ISeq[T]]	=
				if (containsIndex(index))	Some(peer patch (index, ISeq.empty, 1))
				else						None

		/** move an item from a given item index to an inter-item gap */
		def moveAt(fromIndex:Int, toGap:Int):Option[ISeq[T]]	=
				if (containsIndex(fromIndex) && containsGap(toGap)) {
					if (fromIndex < toGap-1) {
						Some(
							peer
							.patch(toGap,		ISeq(peer(fromIndex)),	0)
							.patch(fromIndex,	ISeq.empty,			1)
						)
					}
					else if (fromIndex > toGap) {
						Some(
							peer
							.patch(fromIndex,	ISeq.empty,			1)
							.patch(toGap,		ISeq(peer(fromIndex)),	0)
						)
					}
					else None
				}
				else None

		/** move multiple items item from a given index to a inter-item gap of another index if possible */
		def moveManyAt(fromIndex:Int, count:Int, toGap:Int):Option[ISeq[T]]	=
				if (
					fromIndex	>= 0 && fromIndex+count	<= peer.size &&
					toGap		>= 0 && toGap			<= peer.size
				) {
					Some(
						if (fromIndex < toGap) {
							// move right
							(peer slice (0,						fromIndex))				++
							(peer slice (fromIndex	+ count,	toGap))					++
							(peer slice (fromIndex,				fromIndex	+ count))	++
							(peer slice (toGap,					peer.size))
						}
						else if (fromIndex > toGap) {
							// move left
							(peer slice (0,						toGap))				++
							(peer slice (fromIndex,				fromIndex + count))	++
							(peer slice (toGap,					fromIndex))			++
							(peer slice (fromIndex	+ count,	peer.size))
						}
						else peer
					)
				}
				else None

		def storeAt(index:Int):Option[Store[ISeq[T],T]]	=
				peer lift index map { item =>
					Store[ISeq[T],T](
						item,
						peer updated (index, _)
					)
				}

		def toNesOption:Option[Nes[T]]	=
				Nes fromISeq peer

		private def indexOption(index:Int):Option[Int]	=
				if (index != -1)	Some(index)
				else				None
	}
}
