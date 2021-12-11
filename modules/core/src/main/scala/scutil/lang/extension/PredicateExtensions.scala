package scutil.lang.extension

import scutil.lang.*

object PredicateExtensions {
	implicit final class PredicateExt[T](peer:Predicate[T]) {
		def unary_! :Predicate[T]						= Predicates not	peer
		def && [U<:T](that:Predicate[U]):Predicate[U]	= Predicates.and	(peer, that)
		def || [U<:T](that:Predicate[U]):Predicate[U]	= Predicates.or		(peer, that)

		// non-symbolic aliases
		def not:Predicate[T]							= Predicates not	peer
		def and[U<:T](that:Predicate[U]):Predicate[U]	= Predicates.and	(peer, that)
		def or[U<:T](that:Predicate[U]):Predicate[U]	= Predicates.or		(peer, that)

		//------------------------------------------------------------------------------

		def optionOn[S<:T,U](function:S=>U):S=>Option[U]	=
			it	=> if (peer(it))	Some(function(it))	else None

		def optionNotOn[S<:T,U](function:S=>U):S=>Option[U]	=
			it	=> if (!peer(it))	Some(function(it))	else None

		def flatOptionOn[S<:T,U](function:S=>Option[U]):S=>Option[U]	=
			it	=> if (peer(it))	function(it)	else None

		def flatOptionNotOn[S<:T,U](function:S=>Option[U]):S=>Option[U]	=
			it	=> if (!peer(it))	function(it)	else None

		def eitherOn[S<:T,UL,UR](left:S=>UL, right:S=>UR):S=>Either[UL,UR]	=
			it	=> if (peer(it))	Left(left(it))	else	Right(right(it))

		def validatedOn[S<:T,UL,UR](invalid:S=>UL, valid:S=>UR):S=>Validated[UL,UR]	=
			it	=> if (peer(it))	Validated.invalid(invalid(it))	else	Validated.valid(valid(it))
	}
}
