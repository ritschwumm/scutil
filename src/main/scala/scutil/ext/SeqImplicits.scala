package scutil.ext

import scala.collection.immutable

import scutil.lang._
import scutil.Tuples

object SeqImplicits extends SeqImplicits

trait SeqImplicits {
	implicit def toSeqExt[T](delegate:Seq[T])	= new SeqExt(delegate)
}

final class SeqExt[T](delegate:Seq[T]) {
	def insertAt(index:Int, item:T):Seq[T]	= 
			delegate patch (index, Seq(item), 0)
		
	def removeAt(index:Int):Seq[T]	= 
			delegate patch (index, Seq.empty, 1)
		
	def containsIndex(index:Int):Boolean	=
			index >= 0 && index < delegate.size
		
	/** map the value for a single index */
	def updatedBy(index:Int, func:T=>T):Seq[T]	=
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
			else					Seq.empty
	
	/** insert a separator between elements */	
	def intersperse[U>:T](separator: =>U):Seq[U]	=
			if (delegate.nonEmpty)	delegate flatMap { Seq(separator, _) } tail
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
					Seq(Right(delegate slice (a+1,b))) ++
					((delegate lift b) map Left.apply)
				}
			}
			else {
				Seq.empty
			}

	/** equivalent elements go into own Seqs */
	def equivalentSpans(equivalent:(T,T)=>Boolean):Seq[Seq[T]]	= {
		def impl(in:Seq[T]):Seq[Seq[T]]	= {
			val (a,b)	= in span { equivalent(in.head, _) }
			if (b.nonEmpty)	a +: impl(b)
			else			Seq(a)
		}
		if (delegate.nonEmpty)	impl(delegate)
		else					Seq.empty
	}
	
	/** equivalentSpans on a single property */
	def equivalentSpansBy[U](extract:T=>U):Seq[Seq[T]]	=
			equivalentSpans { extract(_) == extract(_) }
}
