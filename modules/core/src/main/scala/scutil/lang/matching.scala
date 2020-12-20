package scutil.lang

/** allows to match the same value with two patterns at the same time */
object & {
	def unapply[T](value:T):Some[(T,T)]	= Some((value, value))
}
