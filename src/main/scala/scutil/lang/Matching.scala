package scutil.lang

/** allows to match the same value with two patterns at the same time */
object & {
	def unapply[T](value:T):Option[(T,T)]	= Some((value, value)) 
}

// type ->[A,B]	= (A,B)

/** allows pattern matching on tuples constructed with -> */
object -> {
	def unapply[S,T](value:(S,T)):Option[(S,T)]	= Some(value)
}
