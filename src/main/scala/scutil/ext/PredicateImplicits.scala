package scutil.ext

import scutil.Predicates._

object PredicateImplicits extends PredicateImplicits

trait PredicateImplicits {
	implicit def toPredicateExt[T](delegate:Predicate[T]) = new PredicateExt[T](delegate)
}

final class PredicateExt[T](delegate:Predicate[T]) {
	def unary_! :Predicate[T]						= not(delegate)
	def && [U<:T](that:Predicate[U]):Predicate[U]	= and(delegate, that)
	def || [U<:T](that:Predicate[U]):Predicate[U]	= or(delegate, that)
}
