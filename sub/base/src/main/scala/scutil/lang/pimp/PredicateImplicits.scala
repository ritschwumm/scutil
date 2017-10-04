package scutil.lang.pimp

import scutil.lang._

object PredicateImplicits extends PredicateImplicits

trait PredicateImplicits {
	implicit final class PredicateExt[T](peer:Predicate[T]) {
		def unary_! :Predicate[T]						= Predicates not	peer
		def && [U<:T](that:Predicate[U]):Predicate[U]	= Predicates and	(peer, that)
		def || [U<:T](that:Predicate[U]):Predicate[U]	= Predicates or		(peer, that)
		
		// non-symbolic aliases
		def not:Predicate[T]							= Predicates not	peer
		def and[U<:T](that:Predicate[U]):Predicate[U]	= Predicates and	(peer, that)
		def or[U<:T](that:Predicate[U]):Predicate[U]	= Predicates or		(peer, that)
		
		//------------------------------------------------------------------------------
		
		def optionOn[S<:T,U](function:S=>U):PFunction[S,U]	=
				it	=> if (peer(it))	Some(function(it))	else None
			
		def optionNotOn[S<:T,U](function:S=>U):PFunction[S,U]	=
				it	=> if (!peer(it))	Some(function(it))	else None
		
		@deprecated("use optionOn", "0.121.0")
		def guardOn[S<:T,U](function:S=>U):PFunction[S,U]	=
				optionOn(function)
			
		@deprecated("use optionNotOn", "0.121.0")
		def preventOn[S<:T,U](function:S=>U):PFunction[S,U]	=
				optionNotOn(function)
			
		def flatOptionOn[S<:T,U](function:PFunction[S,U]):PFunction[S,U]	=
				it	=> if (peer(it))	function(it)	else None
				
		def flatOptionNotOn[S<:T,U](function:PFunction[S,U]):PFunction[S,U]	=
				it	=> if (!peer(it))	function(it)	else None
				
		@deprecated("use flatOptionOn", "0.121.0")
		def flatGuardOn[S<:T,U](function:PFunction[S,U]):PFunction[S,U]	=
				flatOptionOn(function)
				
		@deprecated("use flatOptionNotOn", "0.121.0")
		def flatPreventOn[S<:T,U](function:PFunction[S,U]):PFunction[S,U]	=
				flatOptionNotOn(function)
				
		def eitherOn[S<:T,UL,UR](left:S=>UL, right:S=>UR):S=>Either[UL,UR]	=
				it	=> if (peer(it))	Left(left(it))	else	Right(right(it))
				
		def validatedOn[S<:T,UL,UR](bad:S=>UL, good:S=>UR):S=>Validated[UL,UR]	=
				it	=> if (peer(it))	Bad(bad(it))	else	Good(good(it))
	}
}
