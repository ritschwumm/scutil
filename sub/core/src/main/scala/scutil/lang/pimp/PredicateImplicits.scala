package scutil.lang.pimp

import scutil.lang._

object PredicateImplicits extends PredicateImplicits

trait PredicateImplicits {
	implicit def toPredicateExt[T](peer:Predicate[T])	= new PredicateExt[T](peer)
}

final class PredicateExt[T](peer:Predicate[T]) {
	def unary_! :Predicate[T]						= Predicates not	peer
	def && [U<:T](that:Predicate[U]):Predicate[U]	= Predicates and	(peer, that)
	def || [U<:T](that:Predicate[U]):Predicate[U]	= Predicates or		(peer, that)
	
	// non-symbolic aliases
	def not:Predicate[T]							= Predicates not	peer
	def and[U<:T](that:Predicate[U]):Predicate[U]	= Predicates and	(peer, that)
	def or[U<:T](that:Predicate[U]):Predicate[U]	= Predicates or		(peer, that)
	
	//------------------------------------------------------------------------------
	
	def guardOn[S<:T,U](function:S=>U):PFunction[S,U]	= 
			it	=> if (peer(it))	Some(function(it))	else None
			
	def preventOn[S<:T,U](function:S=>U):PFunction[S,U]	=
			it	=> if (!peer(it))	Some(function(it))	else None
			
	def flatGuardOn[S<:T,U](function:PFunction[S,U]):PFunction[S,U]	=
			it	=> if (peer(it))	function(it)	else None
			
	def flatPreventOn[S<:T,U](function:PFunction[S,U]):PFunction[S,U]	=
			it	=> if (!peer(it))	function(it)	else None
			
	def eitherOn[S<:T,UL,UR](left:S=>UL, right:S=>UR):S=>Either[UL,UR]	=
			it	=> if (peer(it))	Left(left(it))	else	Right(right(it))
			
	def triedOn[S<:T,UF,UW](fail:S=>UF, win:S=>UW):S=>Tried[UF,UW]	=
			it	=> if (peer(it))	Fail(fail(it))	else	Win(win(it))
}
