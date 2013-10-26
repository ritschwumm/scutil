package scutil.pimp

import scala.collection.immutable

import scutil.lang._

object SeqImplicits extends SeqImplicits

trait SeqImplicits {
	implicit def toSeqExt[T](peer:Seq[T])	= new SeqExt(peer)
}

final class SeqExt[T](peer:Seq[T]) {
	/** allow inserting an item at a given index if possible */
	def insertionAt(index:Int):Option[T=>Seq[T]]	=
			if (index >= 0 && index <= peer.size) {
				Some { item => peer patch (index, Seq(item), 0) }
			}
			else None
			
	/** insert an item at a given index if possible */
	def insertAt(index:Int, item:T):Option[Seq[T]]	=
			if (index >= 0 && index <= peer.size) {
				Some(peer patch (index, Seq(item), 0))
			}
			else None
		
	/** remove the item at a given index if possible */
	def removeAt(index:Int):Option[Seq[T]]	=
			if (index >= 0 && index < peer.size) {
				Some(peer patch (index, Seq.empty, 1))
			}
			else None
		
	/** move an item from a given index to a inter-item gap of another index if possible */
	def moveAt(from:Int, to:Int):Option[Seq[T]]	=
			if (from < to-1) {
				Some(
					peer 
					.patch(to,		Seq(peer(from)),	0) 
					.patch(from,	Seq.empty,			1)
				)
			}
			else if (from > to) {
				Some(
					peer 
					.patch(from,	Seq.empty,			1) 
					.patch(to,		Seq(peer(from)),	0)
				)
			}
			else None
	
	/** concatenate the Seq with itself n times */
	def times(count:Int):Seq[T]	=
			(0 until count).toVector flatMap { _ => peer }
	
	/** true if the index designates an element of the Seq */
	def containsIndex(index:Int):Boolean	=
			index >= 0 && index < peer.size
		
	/** get value at an index or return a default value */
	def liftOrElse(index:Int, default:T):T	=
			peer lift index getOrElse default
		
	/** like find but searching from the end to the front */
	def findLast(predicate:Predicate[T]):Option[T]	=
			peer.reverse find predicate

	/** like collectFirst but searching from the end to the front */		
	def collectLast[U](pf:PartialFunction[T,U]):Option[U]	=
			peer.reverse collectFirst pf
	
	/** like collectFirst but using a PFunction */
	def collapseFirst[U](find:PFunction[T,U]):Option[U]	= {
		peer foreach { it =>
			val out	= find(it)
			if (out.isDefined)	return out
		}
		return None
	}
	
	/** like collectLast but using a PFunction */
	def collapseLast[U](find:PFunction[T,U]):Option[U]	= {
		peer.reverse foreach { it =>
			val out	= find(it)
			if (out.isDefined)	return out
		}
		return None
	}
	
	def tailOption:Option[Seq[T]]	=
			if (peer.nonEmpty)	Some(peer.tail)
			else				None
	
	def initOption:Option[Seq[T]]	=
			if (peer.nonEmpty)	Some(peer.init)
			else				None
	
	/** get item at index and the Seq without that element if possible */
	def extractAt(index:Int):Option[(T,Seq[T])]	=
			peer lift index map { it =>
				(it, peer patch (index, Seq.empty, 1))
			}
		
	/** get first item and rest of the Seq if possible */
	def extractHead:Option[(T,Seq[T])]	=
			if (peer.nonEmpty)	Some((peer.head, peer.tail))
			else				None
		
	/** get last item and rest of the Seq if possible */
	def extractLast:Option[(T,Seq[T])]	=
			if (peer.nonEmpty)	Some((peer.last, peer.init))
			else					None
		
	/** map the value for a single index */
	def updatedBy(index:Int, func:Endo[T]):Seq[T]	=
			if (index >= 0 && index < peer.size)	peer updated (index, func(peer(index)))
			else									peer

	/** distinct with a custom equality check */ 
	def distinctWith(same:(T,T)=>Boolean):Seq[T] = 
			((peer foldLeft Seq.empty[T]) { (retained:Seq[T], candidate:T) =>
				retained find { same(_,candidate) } match {
					case Some(_)	=> retained
					case None		=> candidate +: retained
				}
			}).reverse
	
	/** distinct on a single property */ 
	def distinctBy[U](extract:T=>U):Seq[T] = 
			distinctWith { extract(_) == extract(_) }
	
	/** pairwise neighbors */
	def zipTail:Seq[(T,T)]	= 
			if (peer.nonEmpty)	peer zip peer.tail
			else				Vector.empty
	
	/** insert a separator between elements, before the first and after the last item */	
	def surround[U>:T](separator: =>U):Seq[U]	=
			if (peer.nonEmpty)	(peer flatMap { Seq(separator, _) }) :+ separator
			else				peer
		
	/** insert a separator between elements */	
	def intersperse[U>:T](separator: =>U):Seq[U]	=
			if (peer.nonEmpty)	(peer flatMap { Seq(separator, _) }).tail
			else				peer
	
	/** triple every item with its previous and next item */
	def adjacents:Seq[(Option[T],T,Option[T])]	= {
		val somes	= peer map Some.apply
		val prevs	= None +: (somes dropRight 1) 
		val nexts	= (somes drop 1) :+ None
		prevs zip peer zip nexts map Tuples.runcurry3
	}
	
	/** optionally insert something between two items */
	def between(func:(T,T)=>Option[T]):Seq[T]	=
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
	
	/** separators go to the Left, the rest goes into Right Seqs  */
	def splitWhere(separator:Predicate[T]):Seq[Either[T,Seq[T]]] =
			if (peer.nonEmpty) {
				val indizes = peer.zipWithIndex collect { case (t,i) if (separator(t)) => i }
				(-1 +: indizes) zip (indizes :+ peer.size) flatMap { case (a,b) => 
					Vector(Right(peer slice (a+1,b))) ++
					((peer lift b) map Left.apply)
				}
			}
			else Vector.empty

	/** equivalent elements go into own Seqs */
	def equivalentSpans(equivalent:(T,T)=>Boolean):Seq[Seq[T]]	= {
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
}
