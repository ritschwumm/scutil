package scutil.math.extension

object OrderingSyntaxExtensions {
	// NOTE this is the same as Ordering.Implicits.infixOrderingOps but without the ugly type projection
	extension [T:Ordering](peer:T) {
		def < (that:T):Boolean		= compare(that) < 0
		def > (that:T):Boolean		= compare(that) > 0
		def <= (that:T):Boolean		= compare(that) <= 0
		def >= (that:T):Boolean		= compare(that) >= 0

		def equiv(that:T):Boolean	= compare(that) == 0

		def min(that:T):T			= if (<(that))	peer else that
		def max(that:T):T			= if (>=(that))	peer else that

		private def compare(that:T):Int	= summon[Ordering[T]].compare(peer, that)
	}
}
