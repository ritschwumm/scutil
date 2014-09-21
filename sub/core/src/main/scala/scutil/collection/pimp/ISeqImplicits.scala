package scutil.collection.pimp

import scala.collection.immutable

import scutil.lang._

object ISeqImplicits extends ISeqImplicits

trait ISeqImplicits {
	implicit def toISeqExt[T](peer:ISeq[T])	= new ISeqExt(peer)
}

final class ISeqExt[T](peer:ISeq[T]) {
	// aliases with low precendence
	
	def prepend(it:T):ISeq[T]	= it +: peer
	def append(it:T):ISeq[T]	= peer :+ it
	
	def prependAll(it:Traversable[T]):ISeq[T]	= it ++: peer
	def appendAll(it:Traversable[T]):ISeq[T]	= peer ++ it
	
	/** allow inserting an item at a given index if possible */
	def insertionAt(index:Int):Option[T=>ISeq[T]]	=
			if (index >= 0 && index <= peer.size) {
				Some { item => peer patch (index, ISeq(item), 0) }
			}
			else None
			
	/** insert an item at a given index if possible */
	def insertAt(index:Int, item:T):Option[ISeq[T]]	=
			if (index >= 0 && index <= peer.size) {
				Some(peer patch (index, ISeq(item), 0))
			}
			else None
		
	/** remove the item at a given index if possible */
	def removeAt(index:Int):Option[ISeq[T]]	=
			if (index >= 0 && index < peer.size) {
				Some(peer patch (index, ISeq.empty, 1))
			}
			else None
		
	/** move an item from a given index to a inter-item gap of another index if possible */
	def moveAt(from:Int, to:Int):Option[ISeq[T]]	=
			if (from < to-1) {
				Some(
					peer 
					.patch(to,		ISeq(peer(from)),	0) 
					.patch(from,	ISeq.empty,			1)
				)
			}
			else if (from > to) {
				Some(
					peer 
					.patch(from,	ISeq.empty,			1) 
					.patch(to,		ISeq(peer(from)),	0)
				)
			}
			else None
			
	/** move multiple items item from a given index to a inter-item gap of another index if possible */
	def moveManyAt(from:Int, count:Int, to:Int):Option[ISeq[T]]	=
			if (from >= 0 && from+count <= peer.size && to >= 0 && to <= peer.size) {
				Some(
					if (from < to) {
						// move right
						(peer slice (0,					from))				++
						(peer slice (from	+ count,	to))				++
						(peer slice (from,				from	+ count))	++
						(peer slice (to,				peer.size))
					}
					else if (from > to) {
						// move left
						(peer slice (0,					to))			++
						(peer slice (from,				from + count))	++
						(peer slice (to,				from))			++
						(peer slice (from	+ count,	peer.size))
					}
					else peer
				)
			}
			else None
	
	/** concatenate the ISeq with itself n times */
	def times(count:Int):ISeq[T]	=
			(0 until count).toVector flatMap { _ => peer }
	
	/** true if the index designates an element of the ISeq */
	def containsIndex(index:Int):Boolean	=
			index >= 0 && index < peer.size
		
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
	def findLast(predicate:Predicate[T]):Option[T]	=
			peer.reverse find predicate

	/** like collectFirst but searching from the end to the front */		
	def collectLast[U](pf:PartialFunction[T,U]):Option[U]	=
			peer.reverse collectFirst pf
	
	// NOTE this is in TraversableImplicits
	// def collapseFirst[U](find:PFunction[T,U]):Option[U]
	
	/** like collectLast but using a PFunction */
	def collapseLast[U](find:PFunction[T,U]):Option[U]	= {
		peer.reverse foreach { it =>
			val out	= find(it)
			if (out.isDefined)	return out
		}
		return None
	}
	
	def tailOption:Option[ISeq[T]]	=
			if (peer.nonEmpty)	Some(peer.tail)
			else				None
	
	def initOption:Option[ISeq[T]]	=
			if (peer.nonEmpty)	Some(peer.init)
			else				None
	
	/** get item at index and the ISeq without that element if possible */
	def extractAt(index:Int):Option[(T,ISeq[T])]	=
			peer lift index map { it =>
				(it, peer patch (index, ISeq.empty, 1))
			}
		
	/** get first item and rest of the ISeq if possible */
	def extractHead:Option[(T,ISeq[T])]	=
			if (peer.nonEmpty)	Some((peer.head, peer.tail))
			else				None
		
	/** get last item and rest of the ISeq if possible */
	def extractLast:Option[(T,ISeq[T])]	=
			if (peer.nonEmpty)	Some((peer.last, peer.init))
			else				None
		
	def withReverse(func:Endo[ISeq[T]]):ISeq[T]	=
			func(peer.reverse).reverse
		
	/** map the value for a single index */
	def updatedBy(index:Int, func:Endo[T]):ISeq[T]	=
			if (index >= 0 && index < peer.size)	peer updated (index, func(peer(index)))
			else									peer

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
	def zipTail:ISeq[(T,T)]	= 
			if (peer.nonEmpty)	peer zip peer.tail
			else				Vector.empty
	
	/** insert a separator between elements, before the first and after the last item */	
	def surround[U>:T](separator: =>U):ISeq[U]	=
			if (peer.nonEmpty)	(peer flatMap { ISeq(separator, _) }) :+ separator
			else				peer
		
	/** insert a separator between elements */	
	def intersperse[U>:T](separator: =>U):ISeq[U]	=
			if (peer.nonEmpty)	(peer flatMap { ISeq(separator, _) }).tail
			else				peer
	
	/** triple every item with its previous and next item */
	def adjacents:ISeq[(Option[T],T,Option[T])]	= {
		val somes	= peer map Some.apply
		val prevs	= None +: (somes dropRight 1) 
		val nexts	= (somes drop 1) :+ None
		prevs zip peer zip nexts map Tuples.runcurry3
	}
	
	/** optionally insert something between two items */
	def insertBetween(func:(T,T)=>Option[T]):ISeq[T]	=
			if (peer.size < 2)	peer
			else {
				val	out	= new immutable.VectorBuilder[T]
				peer zip peer.tail foreach { case (now,later) =>
					out	+= now
					func(now,later) foreach out.+=
				}
				out	+= peer.last
				out.result
			}
	
	/** separators go to the Left, the rest goes into Right ISeqs  */
	def splitWhere(separator:Predicate[T]):ISeq[Either[T,ISeq[T]]] =
			if (peer.nonEmpty) {
				val indizes = peer.zipWithIndex collect { case (t,i) if (separator(t)) => i }
				(-1 +: indizes) zip (indizes :+ peer.size) flatMap { case (a,b) => 
					Vector(Right(peer slice (a+1,b))) ++
					((peer lift b) map Left.apply)
				}
			}
			else Vector.empty

	/** equivalent elements go into own ISeqs */
	def equivalentSpans(equivalent:(T,T)=>Boolean):ISeq[ISeq[T]]	= {
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
		
	def toNesOption:Option[Nes[T]]	=
			Nes fromISeq peer
		
	private def indexOption(index:Int):Option[Int]	=
			if (index != -1)	Some(index)
			else				None
}
