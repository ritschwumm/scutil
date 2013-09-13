package scutil.lang

import scutil.lang._

object Predicates {
	val constTrue:Any=>Boolean		= _ => true
	val constFalse:Any=>Boolean		= _ => false
	
	def not[T](sub:Predicate[T]):Predicate[T]								= (it:T)	=> !sub(it)
	def and[X,S>:X,T>:X](sub1:Predicate[S], sub2:Predicate[T]):Predicate[X]	= (it:X)	=> sub1(it) && sub2(it)
	def or[X,S>:X,T>:X](sub1:Predicate[S], sub2:Predicate[T]):Predicate[X]	= (it:X)	=> sub1(it) || sub2(it)
}
