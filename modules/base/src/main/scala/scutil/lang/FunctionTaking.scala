package scutil.lang

/** type inference helper */
final class FunctionTaking[S] {
	def apply[T](func:S=>T):S=>T	= func
}
