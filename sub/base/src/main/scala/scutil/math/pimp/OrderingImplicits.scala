package scutil.math.pimp

object OrderingImplicits extends OrderingImplicits

trait OrderingImplicits {
	implicit final class OrderingExt[T](peer:Ordering[T]) {
		/** Ordering should be contravariant */
		def vary[U<:T]:Ordering[U]	=
				new Ordering[U] {
					def compare(x:U, y:U):Int	= {
						val xx:T = x
						peer compare (x, y)
					}
				}
				
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
