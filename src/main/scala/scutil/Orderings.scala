package scutil

object Orderings {
	/** 
	useful with case classes where unapply always returns a Some,
	fails with an Exception if None is returned.
	*/
	def bySome[S,T:Ordering](unapplyFunc:S=>Option[T]):Ordering[S]	=
			Ordering by (unapplyFunc andThen { _.get })
}
