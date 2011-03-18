package scutil

object Predicate {
	implicit def predicate[T](func:T=>Boolean):Predicate[T] = new Predicate[T](func)
	
	val always:Any=>Boolean	= _ => true
	val never:Any=>Boolean	= _ => false
}

final class Predicate[-T](func:T=>Boolean) {
	def unary_! :T=>Boolean						= (it:T) => !func(it)
	def && [U<:T](that:U=>Boolean):U=>Boolean	= (it:U) => func(it) && that(it)
	def || [U<:T](that:U=>Boolean):U=>Boolean	= (it:U) => func(it) || that(it)
}
