package scutil.validation

import scutil.structure._

object Validated {
	def valid[P,T](value:T):Validated[P,T]		= Valid(value)
	def invalid[P,T](problem:P):Validated[P,T]	= Invalid(problem)
	
	def when[P,T](condition:Boolean, value: =>T, problem: => P):Validated[P,T]	= condition match {
		case true	=> Valid(value)
		case false	=> Invalid(problem)
	}
	
	def whenSome[P,T](value:Option[T], problem: =>P):Validated[P,T]	= 
			value map Valid.apply getOrElse Invalid(problem)
	
	def whenRight[P,T](value:Either[P,T]):Validated[P,T]	= 
			value fold (Invalid.apply, Valid.apply) 
}

/** 
A kind of right-leaning Either. This is a Monad and has a special Applicative accumulating errors.
All operations work on the Valid case, for the Invalid case use the InvalidProjection.
The problem type P is expected to be a Semigroup (via implicit conversion)
to enable accumulation of errors. The value type T is unrestricted.
*/
sealed abstract class Validated[+P,+T] {
	/** catamorphism used for pattern matching  */
	def cata[X](valueFunc:T=>X, problemFunc:P=>X):X
	
	def isValid:Boolean		= cata(_ => true, _ => false)
	def isInvalid:Boolean	= cata(_ => false, _ => true)
	
	def foreach(func:T=>Unit):Unit								= cata(func,						identity)
	def map[U](func:T=>U):Validated[P,U]						= cata(func andThen Valid.apply,	Invalid.apply)
	def flatMap[Q>:P,U](func:T=>Validated[Q,U]):Validated[Q,U]	= cata(func,						Invalid.apply)
	
	/** function application within the "useful" Applicative (which is not the one derived from the Monad!) */
	def ap[Q>:P,U,V](that:Validated[Q,U])(implicit sg:Semigroup[Q], witness:T=>U=>V):Validated[Q,V]	= 
			that pa (this map witness)
	
	def pa[Q>:P,U](that:Validated[Q,T=>U])(implicit sg:Semigroup[Q]):Validated[Q,U]	= 
			this cata (
				value1 => that cata (
					value2		=> Valid(value2(value1)), 
					problem2	=> Invalid(problem2)
				),
				problem1 => that cata (
					value2		=> Invalid(problem1),
					problem2	=> Invalid(sg append (problem1, problem2))
				)
			)
	/*
	// alternative implementation of pa using pattern matching instead of cata
	def pa[Q>:P,U](that:Validated[Q,T=>U]):Validated[Q,U]	= (this,that) match {
		case (Valid(value1), 		Valid(value2))		=> Valid(value2(value1))
		case (Valid(value1),		Invalid(problem2))	=> Invalid(problem2)
		case (Invalid(problem1),	Valid(value2))		=> Invalid(problem1)
		case (Invalid(problem1),	Invalid(problem2))	=> Invalid(problem1 ++ problem2)
	}
	*/
	
	def pair[Q>:P,U](that:Validated[Q,U])(implicit sg:Semigroup[Q]):Validated[Q,(T,U)]	=
			// pure Pair ap this ap that === this map Pair ap that
			// (Valid(Pair[T,U] _ curried):Validated[Q,T=>U=>Pair[T,U]]) ap this ap that
			this map (Pair[T,U] _ curried) ap that
	
	def orElse[Q>:P,U>:T](elseValue: =>Validated[Q,U]):Validated[Q,U]	= cata(Valid.apply,	_ => elseValue)
	def getOrElse[U>:T](elseValue: =>U):U								= cata(identity,	_ => elseValue)
	def toEither:Either[P,T]											= cata(Right.apply,	Left.apply)
	def toOption:Option[T]												= cata(Some.apply,	_ => None)
	def toList:List[T]													= cata(List(_),		_ => Nil)
	
	/** allow operations on the Invalid case of this Validated */
	def invalid:InvalidProjection[P,T]	= new InvalidProjection[P,T](this)
}

/** everything is fine, we have a usable value */
case class Valid[P,T](value:T) extends Validated[P,T] {
	def cata[X](valueFunc:T=>X, problemFunc:P=>X):X	= valueFunc(value)
}
/** at least one problem occurred in a computation */
case class Invalid[P,T](problem:P)	extends Validated[P,T] {
	def cata[X](valueFunc:T=>X, problemFunc:P=>X):X	= problemFunc(problem)
}

/** operations on the Invalid side of a Validated. This is a Monad, too. */
final class InvalidProjection[+P,+T](delegate:Validated[P,T]) {
	// TODO pa/ap
	def foreach(func:P=>Unit):Unit										= delegate cata (identity,			func)
	def map[Q](func:P=>Q):Validated[Q,T]								= delegate cata (Valid.apply,		func andThen Invalid.apply)
	def flatMap[Q,U>:T](func:P=>Validated[Q,U]):Validated[Q,U]			= delegate cata (Valid.apply,		func)
	
	def orElse[U>:T,Q>:P](elseValue: =>Validated[Q,U]):Validated[Q,U]	= delegate cata (_ => elseValue,	Invalid.apply)
	def getOrElse[Q>:P](elseValue: =>Q):Q								= delegate cata (_ => elseValue,	identity)
	def toEither:Either[T,P]											= delegate cata (Left.apply,		Right.apply)
	def toOption:Option[P]												= delegate cata (_ => None,			Some.apply)
	def toList:List[P]													= delegate cata (_ => Nil,			List(_))
}
