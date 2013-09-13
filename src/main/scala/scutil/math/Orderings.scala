package scutil.math

import scutil.lang._

object Orderings {
	/** 
	useful with case classes where unapply always returns a Some,
	fails with an Exception if None is returned.
	*/
	def bySome[S,T:Ordering](unapplyFunc:PFunction[S,T]):Ordering[S]	=
			Ordering by (unapplyFunc andThen { _.get })
}
