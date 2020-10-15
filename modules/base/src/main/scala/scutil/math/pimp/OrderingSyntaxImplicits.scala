package scutil.math.pimp

object OrderingSyntaxImplicits extends OrderingSyntaxImplicits

trait OrderingSyntaxImplicits {
	// NOTE this is the same as Ordering.Implicits.infixOrderingOps but without the ugly type projection
	implicit final class OrderingSyntaxExt[T:Ordering](peer:T) {
		private def compare(that:T):Int	= implicitly[Ordering[T]].compare(peer, that)
		def equiv(that:T):Boolean	= compare(that) == 0
		def < (that:T):Boolean		= compare(that) < 0
		def > (that:T):Boolean		= compare(that) > 0
		def <= (that:T):Boolean		= compare(that) <= 0
		def >= (that:T):Boolean		= compare(that) >= 0
		def min(that:T):T	= if (<(that))	peer else that
		def max(that:T):T	= if (>=(that))	peer else that
	}
}
