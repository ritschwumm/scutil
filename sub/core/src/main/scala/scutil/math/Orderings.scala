package scutil.math

import scutil.lang._

object Orderings {
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
