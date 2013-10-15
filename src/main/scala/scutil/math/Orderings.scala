package scutil.math

import scutil.lang._

object Orderings {
	// NOTE Ordering is not contravariant for some reason
	
	/** treats everything as equal */
	def trivial[T]:Ordering[T]	=
			new Ordering[T] {
				def compare(x:T, y:T):Int	= 0
			}
	/** 
	useful with case classes where unapply always returns a Some,
	fails with an Exception if None is returned.
	*/
	def bySome[S,T:Ordering](unapplyFunc:PFunction[S,T]):Ordering[S]	=
			Ordering by (unapplyFunc andThen { _.get })
}
