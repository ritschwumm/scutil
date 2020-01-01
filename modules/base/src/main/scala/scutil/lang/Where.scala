package scutil.lang

import scutil.lang.tc._

object Where {
	def here[A,B](a:A):Where[A,B]		= Here(a)
	def there[A,B](b:B):Where[A,B]		= There(b)
	def both[A,B](a:A, b:B):Where[A,B]	= Both(a,b)

	def maybeHere[A,B](a:Option[A], b:B):Where[A,B]	=
		a match {
			case Some(aa)	=> Both(aa, b)
			case None		=> There(b)
		}

	def maybeThere[A,B](a:A, b:Option[B]):Where[A,B]	=
		b match {
			case Some(bb)	=> Both(a, bb)
			case None		=> Here(a)
		}

	def from[A,B](here:Option[A], there:Option[B]):Option[Where[A,B]]	=
		(here, there) match {
			case (Some(a),	Some(b))	=> Some(Both(a,b))
			case (Some(a),	None)		=> Some(Here(a))
			case (None,		Some(b))	=> Some(There(b))
			case (None,		None)		=> None
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def WhereFunctor[A]:Functor[Where[A,?]]	=
		new Functor[Where[A,?]] {
			override def map[B,BB](it:Where[A,B])(func:B=>BB):Where[A,BB]	= it mapThere func
		}
}

sealed trait Where[+A,+B] {
	def cata[X](here:A=>X, there:B=>X, both:(A,B)=>X):X	=
		this match {
			case Here(a)	=> here(a)
			case There(b)	=> there(b)
			case Both(a,b)	=> both(a, b)
		}

	def hereOption:Option[A]	=
		this match {
			case Here(a)	=> Some(a)
			case Both(a,_)	=> Some(a)
			case There(_)	=> None
		}
	def thereOption:Option[B]	=
		this match {
			case There(b)	=> Some(b)
			case Both(_,b)	=> Some(b)
			case Here(_)	=> None
		}

	def certainlyHere[AA](a:AA):Where[AA,B]	=
		Where maybeThere (a, thereOption)

	def certainlyThere[BB](b:BB):Where[A,BB]	=
		Where maybeHere (hereOption, b)

	def mapHere[X](func:A=>X):Where[X,B]	=
		this match {
			case Here(x)	=> Here(func(x))
			case There(x)	=> There(x)
			case Both(x,b)	=> Both(func(x), b)
		}

	def mapThere[X](func:B=>X):Where[A,X]	=
		this match {
			case Here(x)	=> Here(x)
			case There(x)	=> There(func(x))
			case Both(a,x)	=> Both(a, func(x))
		}

	def bimap[AA,BB](funcA:A=>AA, funcB:B=>BB):Where[AA,BB]	=
		this match {
			case Here(a)	=> Here(funcA(a))
			case There(b)	=> There(funcB(b))
			case Both(a,b)	=> Both(funcA(a), funcB(b))
		}

	def swap:Where[B,A]	=
		this match {
			case Here(a)	=> There(a)
			case There(b)	=> Here(b)
			case Both(a,b)	=> Both(b,a)
		}
}
final case class Here[A,B](here:A)			extends Where[A,B]
final case class There[A,B](there:B)		extends Where[A,B]
final case class Both[A,B](here:A, there:B)	extends Where[A,B]
