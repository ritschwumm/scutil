package scutil.lang

import scutil.lang.tc.*

object Ior {
	def left[A,B](a:A):Ior[A,B]			= Left(a)
	def right[A,B](b:B):Ior[A,B]		= Right(b)
	def both[A,B](a:A, b:B):Ior[A,B]	= Both(a,b)

	def maybeLeft[A,B](a:Option[A], b:B):Ior[A,B]	=
		a match {
			case Some(aa)	=> Both(aa, b)
			case None		=> Right(b)
		}

	def maybeRight[A,B](a:A, b:Option[B]):Ior[A,B]	=
		b match {
			case Some(bb)	=> Both(a, bb)
			case None		=> Left(a)
		}

	def from[A,B](left:Option[A], right:Option[B]):Option[Ior[A,B]]	=
		(left, right) match {
			case (Some(a),	Some(b))	=> Some(Both(a,b))
			case (Some(a),	None)		=> Some(Left(a))
			case (None,		Some(b))	=> Some(Right(b))
			case (None,		None)		=> None
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	given IorFunctor[A]:Functor[Ior[A,_]]	=
		new Functor[Ior[A,_]] {
			override def map[B,BB](it:Ior[A,B])(func:B=>BB):Ior[A,BB]	= it.mapRight(func)
		}
}

enum Ior[+A,+B] {
	case Left[A,B](value:A)			extends Ior[A,B]
	case Right[A,B](value:B)		extends Ior[A,B]
	case Both[A,B](left:A, right:B)	extends Ior[A,B]

	def cata[X](left:A=>X, right:B=>X, both:(A,B)=>X):X	=
		this match {
			case Ior.Left(a)	=> left(a)
			case Ior.Right(b)	=> right(b)
			case Ior.Both(a,b)	=> both(a, b)
		}

	def leftOption:Option[A]	=
		this match {
			case Ior.Left(a)	=> Some(a)
			case Ior.Both(a,_)	=> Some(a)
			case Ior.Right(_)	=> None
		}

	def rightOption:Option[B]	=
		this match {
			case Ior.Right(b)	=> Some(b)
			case Ior.Both(_,b)	=> Some(b)
			case Ior.Left(_)	=> None
		}

	def certainlyLeft[AA](a:AA):Ior[AA,B]	=
		Ior.maybeRight(a, rightOption)

	def certainlyRight[BB](b:BB):Ior[A,BB]	=
		Ior.maybeLeft(leftOption, b)

	def mapLeft[X](func:A=>X):Ior[X,B]	=
		this match {
			case Ior.Left(x)	=> Ior.left(func(x))
			case Ior.Right(x)	=> Ior.right(x)
			case Ior.Both(x,b)	=> Ior.both(func(x), b)
		}

	def mapRight[X](func:B=>X):Ior[A,X]	=
		this match {
			case Ior.Left(x)	=> Ior.left(x)
			case Ior.Right(x)	=> Ior.right(func(x))
			case Ior.Both(a,x)	=> Ior.both(a, func(x))
		}

	def bimap[AA,BB](funcA:A=>AA, funcB:B=>BB):Ior[AA,BB]	=
		this match {
			case Ior.Left(a)	=> Ior.left(funcA(a))
			case Ior.Right(b)	=> Ior.right(funcB(b))
			case Ior.Both(a,b)	=> Ior.both(funcA(a), funcB(b))
		}

	def swap:Ior[B,A]	=
		this match {
			case Ior.Left(a)	=> Ior.right(a)
			case Ior.Right(b)	=> Ior.left(b)
			case Ior.Both(a,b)	=> Ior.both(b,a)
		}
}
