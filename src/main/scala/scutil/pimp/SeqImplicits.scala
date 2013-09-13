package scutil.pimp

import scala.collection.immutable

import scutil.lang._

object SeqImplicits extends SeqImplicits

trait SeqImplicits {
	implicit def toSeqExt[T](delegate:Seq[T])	= new SeqExt(delegate)
}

final class SeqExt[T](delegate:Seq[T]) {
	/** allow inserting an item at a given index if possible */
	def insertionAt(index:Int):Option[T=>Seq[T]]	=
			if (index >= 0 && index <= delegate.size) {
				Some { item => delegate patch (index, Seq(item), 0) }
			}
			else None
			
	/** insert an item at a given index if possible */
	def insertAt(index:Int, item:T):Option[Seq[T]]	=
			if (index >= 0 && index <= delegate.size) {
				Some(delegate patch (index, Seq(item), 0))
			}
			else None
		
	/** remove the item at a given index if possible */
	def removeAt(index:Int):Option[Seq[T]]	=
			if (index >= 0 && index < delegate.size) {
				Some(delegate patch (index, Seq.empty, 1))
			}
			else None
		
	/** move an item from a given index to a inter-item gap of another index if possible */
	def moveAt(from:Int, to:Int):Option[Seq[T]]	=
			if (from < to-1) {
				Some(
					delegate 
					.patch(to,		Seq(delegate(from)),	0) 
					.patch(from,	Seq.empty,				1)
				)
			}
			else if (from > to) {
				Some(delegate 
					.patch(from,	Seq.empty,				1) 
					.patch(to,		Seq(delegate(from)),	0)
				)
			}
			else None
	
	/** concatenate the Seq with itself n times */
	def times(count:Int):Seq[T]	=
			(0 until count).toVector flatMap { _ => delegate }
	
	/** true if the index designates an element of the Seq */
	def containsIndex(index:Int):Boolean	=
			index >= 0 && index < delegate.size
		
	/** get value at an index or return a default value */
	def liftOrElse(index:Int, default:T):T	=
			delegate lift index getOrElse default
		
	/** like find but searching from the end to the front */
	def findLast(predicate:Predicate[T]):Option[T]	=
			delegate.reverse find predicate

	/** like collectFirst but searching from the end to the front */		
	def collectLast[U](pf:PartialFunction[T,U]):Option[U]	=
			delegate.reverse collectFirst pf
	
	/** like collectFirst but using a PFunction */
	def collapseFirst[U](find:PFunction[T,U]):Option[U]	= {
		delegate foreach { it =>
			val out	= find(it)
			if (out.isDefined)	return out
		}
		return None
	}
	
	/** like collectLast but using a PFunction */
	def collapseLast[U](find:PFunction[T,U]):Option[U]	= {
		delegate.reverse foreach { it =>
			val out	= find(it)
			if (out.isDefined)	return out
		}
		return None
	}
	
	/** get item at index and the Seq without that element if possible */
	def extractAt(index:Int):Option[(T,Seq[T])]	=
			delegate lift index map { it =>
				(it, delegate patch (index, Seq.empty, 1))
			}
		
	/** get first item and rest of the Seq if possible */
	def extractHead:Option[(T,Seq[T])]	=
			if (delegate.nonEmpty)	Some((delegate.head,delegate.tail))
			else					None
		
	/** get last item and rest of the Seq if possible */
	def extractLast:Option[(T,Seq[T])]	=
			if (delegate.nonEmpty)	Some((delegate.last,delegate.init))
			else					None
		
	/** map the value for a single index */
	def updatedBy(index:Int, func:Endo[T]):Seq[T]	=
			if (index >= 0 && index < delegate.size)	delegate updated (index, func(delegate(index)))
			else										delegate

	/** distinct with a custom equality check */ 
	def distinctWith(same:(T,T)=>Boolean):Seq[T] = 
			((delegate.toList foldLeft List.empty[T]) { (retained:List[T],candidate:T) =>
				retained find { same(_,candidate) } match {
					case Some(_)	=> retained
					case None		=> candidate :: retained
				}
			}).reverse
	
	/** distinct on a single property */ 
	def distinctBy[U](extract:T=>U):Seq[T] = 
			distinctWith { extract(_) == extract(_) }
	
	/** pairwise neighbors */
	def zipTail:Seq[(T,T)]	= 
			if (delegate.nonEmpty)	delegate zip delegate.tail
			else					Vector.empty
	
	/** insert a separator between elements, before the first and after the last item */	
	def surround[U>:T](separator: =>U):Seq[U]	=
			if (delegate.nonEmpty)	(delegate flatMap { Seq(separator, _) }) :+ separator
			else					delegate
		
	/** insert a separator between elements */	
	def intersperse[U>:T](separator: =>U):Seq[U]	=
			if (delegate.nonEmpty)	(delegate flatMap { Seq(separator, _) }).tail
			else					delegate
	
	/** triple every item with its previous and next item */
	def adjacents:Seq[(Option[T],T,Option[T])]	= {
		val somes	= delegate map Some.apply
		val prevs	= None +: (somes dropRight 1) 
		val nexts	= (somes drop 1) :+ None
		prevs zip delegate zip nexts map Tuples.runcurry3
	}
	
	/** optionally insert something between two items */
	def between(func:(T,T)=>Option[T]):Seq[T]	= {
		if (delegate.size < 2)	return delegate
		val	out	= new immutable.VectorBuilder[T]
		delegate zip delegate.tail foreach { case (now,later) =>
			out	+= now
			func(now,later) foreach out.+=
		}
		out	+= delegate.last
		out.result
	}
	
	/** separators go to the Left, the rest goes into Right Seqs  */
	def splitWhere(separator:Predicate[T]):Seq[Either[T,Seq[T]]] =
			if (delegate.nonEmpty) {
				val indizes = delegate.zipWithIndex collect { case (t,i) if (separator(t)) => i }
				(-1 +: indizes) zip (indizes :+ delegate.size) flatMap { case (a,b) => 
					Vector(Right(delegate slice (a+1,b))) ++
					((delegate lift b) map Left.apply)
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
		if (delegate.nonEmpty)	impl(delegate)
		else					Vector.empty
	}
	
	/** equivalentSpans on a single property */
	def equivalentSpansBy[U](extract:T=>U):Seq[Seq[T]]	=
			equivalentSpans { extract(_) == extract(_) }
}
