package scutil

/** allows to match the same value with two patterns at the same time */
object & {
	def unapply[T](value:T)	= Some(value, value) 
}
