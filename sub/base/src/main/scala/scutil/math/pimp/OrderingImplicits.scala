package scutil.math.pimp

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
	}
	
	implicit final class OrderingExt[T](peer:Ordering[T]) {
		/** Ordering should be contravariant */
		def vary[U<:T]:Ordering[U]	=
				new Ordering[U] {
					def compare(x:U, y:U):Int	= {
						peer compare (x, y)
					}
				}
			
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
