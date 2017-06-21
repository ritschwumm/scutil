package scutil.math.pimp

object OrderingSyntaxImplicits extends OrderingSyntaxImplicits

trait OrderingSyntaxImplicits {
	implicit final class OrderingSyntaxExt[T:Ordering](peer:T) {
		private def compare(that:T):Int	= implicitly[Ordering[T]] compare (peer, that)
		def < (that:T):Boolean	= compare(that) < 0
		def > (that:T):Boolean	= compare(that) > 0
		def <= (that:T):Boolean	= compare(that) <= 0
		def >= (that:T):Boolean	= compare(that) >= 0
		def min(that:T):T	= if (this <= that) peer else that
		def max(that:T):T	= if (this >= that) peer else that
	}
}
