package scutil.math.pimp

import scala.annotation.tailrec

import scutil.lang._

object OrderingImplicits extends OrderingImplicits

trait OrderingImplicits {
	implicit final class OrderingCompanionExt(peer:Ordering.type) {
		/**
		treats everything as equal,
		forms a Monoid with OrderingExt#orElse
		*/
		def trivial[T]:Ordering[T]	=
				new Ordering[T] {
					def compare(x:T, y:T):Int	= 0
				}
				
		/**
		useful with case classes where unapply always returns a Some,
		fails with an Exception if None is returned.
		*/
		def bySome[S,T:Ordering](unapplyFunc:PFunction[S,T]):Ordering[S]	=
				Ordering by (unapplyFunc andThen { it =>
					(it fold (sys error "unexpected None"))(identity)
				})
				
		/** orders lexicographically from left to right */
		def sequence[T](missingFirst:Boolean)(implicit base:Ordering[T]):Ordering[ISeq[T]]	=
			new Ordering[ISeq[T]] {
				def compare(a:ISeq[T], b:ISeq[T]):Int	= {
					@tailrec
					def loop(index:Int):Int	=
							(a lift index, b lift index) match {
								case (Some(aa), Some(bb))	=>
									val tmp	= base compare (aa, bb)
									if (tmp != 0)	tmp
									else			loop(index+1)
								case (Some(aa), None)	=>
									if (missingFirst) +1 else -1
								case (None, Some(bb))	=>
									if (missingFirst) -1 else +1
								case (None, None)	=>
									0
							}
					loop(0)
				}
			}
	}
	
	implicit final class OrderingExt[T](peer:Ordering[T]) {
		/** Ordering should be contravariant */
		def vary[U<:T]:Ordering[U]	=
				new Ordering[U] {
					def compare(x:U, y:U):Int	= {
						peer compare (x, y)
					}
				}
				
		def contraMap[S](func:S=>T):Ordering[S]	=
				peer on func
			
		// TODO remove when scala 2.13 is there
		/**
		alternative to the implicit (Ordering[T],Ordering[T]) => Ordering[T] conversion in Ordering.Implicits,
		forms a Monoid with Orderings#trivial
		*/
		def orElse[U<:T](that:Ordering[U]):Ordering[U]	=
				new Ordering[U] {
					def compare(x:U, y:U):Int	= {
						val	high	= peer compare (x,y)
						if (high != 0) high else that compare (x,y)
					}
				}
	}
}
