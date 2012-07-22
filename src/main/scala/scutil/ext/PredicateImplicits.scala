package scutil.ext

import scutil.Functions._
import scutil.Predicates._
import scutil.data._

object PredicateImplicits extends PredicateImplicits

trait PredicateImplicits {
	implicit def toPredicateExt[T](delegate:Predicate[T])	= new PredicateExt[T](delegate)
}

final class PredicateExt[T](delegate:Predicate[T]) {
	def unary_! :Predicate[T]						= not(delegate)
	def && [U<:T](that:Predicate[U]):Predicate[U]	= and(delegate, that)
	def || [U<:T](that:Predicate[U]):Predicate[U]	= or(delegate, that)
	
	def guardOn[S<:T,U](function:S=>U):S=>Option[U]	= 
			it	=> if (delegate(it))	Some(function(it))	else None
			
	def preventOn[S<:T,U](function:S=>U):S=>Option[U]	=
			it	=> if (!delegate(it))	Some(function(it))	else None
			
	def flatGuardOn[S<:T,U](function:S=>Option[U]):S=>Option[U]	=
			it	=> if (delegate(it))	function(it)	else None
			
	def flatPreventOn[S<:T,U](function:S=>Option[U]):S=>Option[U]	=
			it	=> if (!delegate(it))	function(it)	else None
			
	def eitherOn[S<:T,UL,UR](left:S=>UL, right:S=>UR):S=>Either[UL,UR]	=
			it	=> if (delegate(it))	Left(left(it))	else	Right(right(it))
			
	def triedOn[S<:T,UF,UW](fail:S=>UF, win:S=>UW):S=>Tried[UF,UW]	=
			it	=> if (delegate(it))	Fail(fail(it))	else	Win(win(it))
}
