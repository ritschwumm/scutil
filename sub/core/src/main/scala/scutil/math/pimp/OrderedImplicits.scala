package scutil.math.pimp

object OrderedImplicits extends OrderedImplicits

trait OrderedImplicits {
	implicit def toOrderedExt[T<:Ordered[T]](peer:T) = new OrderedExt[T](peer)
}

final class OrderedExt[T<:Ordered[T]](peer:T) {
	def min(that:T):T	= if (peer <= that) peer else that
	def max(that:T):T	= if (peer >= that) peer else that
}
