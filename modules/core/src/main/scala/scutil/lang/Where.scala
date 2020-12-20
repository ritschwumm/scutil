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

	implicit def WhereFunctor[A]:Functor[Where[A,*]]	=
		new Functor[Where[A,*]] {
			override def map[B,BB](it:Where[A,B])(func:B=>BB):Where[A,BB]	= it mapThere func
		}

	//------------------------------------------------------------------------------

	final case class Here[A,B](here:A)			extends Where[A,B]
	final case class There[A,B](there:B)		extends Where[A,B]
	final case class Both[A,B](here:A, there:B)	extends Where[A,B]
}

sealed trait Where[+A,+B] {
	def cata[X](here:A=>X, there:B=>X, both:(A,B)=>X):X	=
		this match {
			case Where.Here(a)		=> here(a)
			case Where.There(b)		=> there(b)
			case Where.Both(a,b)	=> both(a, b)
		}

	def hereOption:Option[A]	=
		this match {
			case Where.Here(a)		=> Some(a)
			case Where.Both(a,_)	=> Some(a)
			case Where.There(_)		=> None
		}

	def thereOption:Option[B]	=
		this match {
			case Where.There(b)		=> Some(b)
			case Where.Both(_,b)	=> Some(b)
			case Where.Here(_)		=> None
		}

	def certainlyHere[AA](a:AA):Where[AA,B]	=
		Where.maybeThere(a, thereOption)

	def certainlyThere[BB](b:BB):Where[A,BB]	=
		Where.maybeHere(hereOption, b)

	def mapHere[X](func:A=>X):Where[X,B]	=
		this match {
			case Where.Here(x)		=> Where.here(func(x))
			case Where.There(x)		=> Where.there(x)
			case Where.Both(x,b)	=> Where.both(func(x), b)
		}

	def mapThere[X](func:B=>X):Where[A,X]	=
		this match {
			case Where.Here(x)		=> Where.here(x)
			case Where.There(x)		=> Where.there(func(x))
			case Where.Both(a,x)	=> Where.both(a, func(x))
		}

	def bimap[AA,BB](funcA:A=>AA, funcB:B=>BB):Where[AA,BB]	=
		this match {
			case Where.Here(a)		=> Where.here(funcA(a))
			case Where.There(b)		=> Where.there(funcB(b))
			case Where.Both(a,b)	=> Where.both(funcA(a), funcB(b))
		}

	def swap:Where[B,A]	=
		this match {
			case Where.Here(a)		=> Where.there(a)
			case Where.There(b)		=> Where.here(b)
			case Where.Both(a,b)	=> Where.both(b,a)
		}
}

