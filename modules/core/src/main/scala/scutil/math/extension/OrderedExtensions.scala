package scutil.math.extension

object OrderedExtensions {
	// TODO shouldn't these just come from OrderingSyntaxExtensions?
	extension [T<:Ordered[T]](peer:T) {
		def equiv(that:T):Boolean	= (peer compare that) == 0

		def min(that:T):T			= if (peer <= that) peer else that
		def max(that:T):T			= if (peer >= that) peer else that
	}
}
