package scutil.lang

import scutil.lang.tc.*

object Validated {
	def valid[E,T](value:T):Validated[E,T]		= Valid(value)
	def invalid[E,T](problems:E):Validated[E,T]	= Invalid(problems)

	def invalidNes[E,T](problems:E):Validated[Nes[E],T]	= Invalid(Nes one problems)

	//------------------------------------------------------------------------------

	def switch[E,T](ok:Boolean, problems: =>E, value: =>T):Validated[E,T]	=
		if (ok)	valid(value)
		else	invalid(problems)

	// TODO dotty use extension
	implicit final class MergeableValidated[T](peer:Validated[T,T]) {
		def merge:T	=
			peer match {
				case Invalid(x)		=> x
				case Valid(x)		=> x
			}
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	given [S:Semigroup]:Applicative[Validated[S,_]]	=
		new Applicative[Validated[S,_]] {
			override def pure[A](it:A):Validated[S,A]										= Validated valid it
			override def ap[A,B](func:Validated[S,A=>B])(it:Validated[S,A]):Validated[S,B]	= func ap it
		}

	given [S:Semigroup,T]:Semigroup[Validated[S,T]]	=
		Semigroup instance (_ or _)
}

enum Validated[+E,+T] {
	case Invalid[E](problems:E)	extends Validated[E,Nothing]
	case Valid[T](value:T)		extends Validated[Nothing,T]

	def cata[X](invalid:E=>X, valid:T=>X):X	=
		this match {
			case Validated.Invalid(x)	=> invalid(x)
			case Validated.Valid(x)		=> valid(x)
		}

	//------------------------------------------------------------------------------

	def isValid:Boolean	=
		this match {
			case Validated.Invalid(x)	=> false
			case Validated.Valid(x)		=> true
		}

	def isInvalid:Boolean	=
		!isValid

	//------------------------------------------------------------------------------

	def exists(pred:Predicate[T]):Boolean	=
		this match {
			case Validated.Invalid(x)	=> false
			case Validated.Valid(x)		=> pred(x)
		}

	def forall(pred:Predicate[T]):Boolean	=
		this match {
			case Validated.Invalid(x)	=> true
			case Validated.Valid(x)		=> pred(x)
		}

	//------------------------------------------------------------------------------

	def iterator:Iterator[T]	=
		this match {
			case Validated.Invalid(x)	=> Iterator.empty
			case Validated.Valid(x)		=> Iterator single x
		}

	def foreach(effect:Effect[T]):Unit	=
		this match {
			case Validated.Invalid(x)	=> ()
			case Validated.Valid(x)		=> effect(x)
		}

	def map[U](func:T=>U):Validated[E,U]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(x)
			case Validated.Valid(x)		=> Validated.valid(func(x))
		}

	def flatMap[EE>:E,U](func:T=>Validated[EE,U]):Validated[EE,U]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(x)
			case Validated.Valid(x)		=> func(x)
		}

	def flatten[EE>:E,U](using ev:T <:< Validated[EE,U]):Validated[EE,U]	=
		flatMap(ev)

	def ap[EE>:E:Semigroup,U,V](that:Validated[EE,U])(using ev:T <:< (U=>V)):Validated[EE,V]	=
		(this map2 that)(_(_))

	def product[EE>:E:Semigroup,U](that:Validated[EE,U]):Validated[EE,(T,U)]	=
		(this map2 that)((_,_))

	def map2[EE>:E,U,V](that:Validated[EE,U])(func:(T,U)=>V)(using cc:Semigroup[EE]):Validated[EE,V]	=
		(this, that) match {
			case (Validated.Invalid(a),	Validated.Valid(_))		=> Validated.Invalid(a)
			case (Validated.Valid(_),	Validated.Invalid(b))	=> Validated.Invalid(b)
			case (Validated.Invalid(a),	Validated.Invalid(b))	=> Validated.Invalid(cc.combine(a, b))
			case (Validated.Valid(a),	Validated.Valid(b))		=> Validated.Valid(func(a, b))
		}

	//------------------------------------------------------------------------------

	def swap:Validated[T,E]	=
		this match {
			case Validated.Invalid(x)	=> Validated.valid(x)
			case Validated.Valid(x)		=> Validated.invalid(x)
		}

	def withSwapped[EE,TT](func:Validated[T,E]=>Validated[TT,EE]):Validated[EE,TT]	=
		func(swap).swap

	def bimap[EE,TT](invalidFunc:E=>EE, validFunc:T=>TT):Validated[EE,TT]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(invalidFunc(x))
			case Validated.Valid(x)		=> Validated.valid(validFunc(x))
		}

	//------------------------------------------------------------------------------

	def invalidMap[EE](func:E=>EE):Validated[EE,T]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(func(x))
			case Validated.Valid(x)		=> Validated.valid(x)
		}

	def invalidFlatMap[EE,TT>:T](func:E=>Validated[EE,TT]):Validated[EE,TT]	=
		this match {
			case Validated.Invalid(x)	=> func(x)
			case Validated.Valid(x)		=> Validated.valid(x)
		}

	def invalidFlatten[EE,TT>:T](using ev:E <:< Validated[EE,TT]):Validated[EE,TT]	=
		invalidFlatMap(ev)

	def invalidToOption:Option[E]	=
		this match {
			case Validated.Invalid(x)	=> Some(x)
			case Validated.Valid(x)		=> None
		}

	//------------------------------------------------------------------------------

	// NOTE cats' orElse drops errors, this is like cats' <+>
	def or[EE>:E,TT>:T](that:Validated[EE,TT])(using cc:Semigroup[EE]):Validated[EE,TT]	=
		(this, that) match {
			case (Validated.Invalid(a),		Validated.Invalid(b))	=> Validated.invalid(cc.combine(a, b))
			case (Validated.Valid(a),	_)							=> Validated.valid(a)
			case (_,						Validated.Valid(b))		=> Validated.valid(b)
		}

	def getOrElse[TT>:T](that: =>TT):TT	=
		this match {
			case Validated.Invalid(x)	=> that
			case Validated.Valid(x)		=> x
		}

	def getOrRescue[TT>:T](func:E=>TT):TT	=
		this match {
			case Validated.Invalid(x)	=> func(x)
			case Validated.Valid(x)		=> x
		}

	def getOrError(s: =>String):T	=
		getOrElse(sys error s)

	def getOrThrow(func:E=>Throwable):T	=
		this match {
			case Validated.Invalid(x)	=> throw func(x)
			case Validated.Valid(x)		=> x
		}

	//------------------------------------------------------------------------------

	def rescue[TT>:T](func:E=>Option[TT]):Validated[E,TT]	=
		this match {
			case Validated.Invalid(x)	=> func(x) map Validated.valid getOrElse Validated.invalid(x)
			case Validated.Valid(x)		=> Validated.valid(x)
		}

	def reject[EE>:E](func:T=>Option[EE]):Validated[EE,T]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(x)
			case Validated.Valid(x)		=> func(x) map Validated.invalid getOrElse Validated.valid(x)
		}

	def validByOr[EE>:E](func:Predicate[T], invalid: =>EE):Validated[EE,T]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(x)
			case Validated.Valid(x)		=> if (func(x)) Validated.valid(x) else Validated.invalid(invalid)
		}

	def validNotByOr[EE>:E](func:Predicate[T], invalid: =>EE):Validated[EE,T]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(x)
			case Validated.Valid(x)		=> if (!func(x)) Validated.valid(x) else Validated.invalid(invalid)
		}

	def collapseOr[EE>:E,TT](func:T=>Option[TT], invalid: =>EE):Validated[EE,TT]	=
		this match {
			case Validated.Invalid(x)	=> Validated.invalid(x)
			case Validated.Valid(x)		=> func(x) map Validated.valid getOrElse Validated.invalid(invalid)
		}

	def collectOr[EE>:E,TT](func:PartialFunction[T,TT], invalid: =>EE):Validated[EE,TT]	=
		collapseOr(func.lift, invalid)

	//------------------------------------------------------------------------------

	def validEffect(effect:Effect[T]):this.type	= {
		this foreach effect
		this
	}

	def invalidEffect(effect:Effect[E]):this.type	= {
		invalidToOption foreach effect
		this
	}

	//------------------------------------------------------------------------------

	def toEither:Either[E,T]	=
		this match {
			case Validated.Invalid(x)	=> Left(x)
			case Validated.Valid(x)		=> Right(x)
		}

	def toOption:Option[T]	=
		this match {
			case Validated.Invalid(x)	=> None
			case Validated.Valid(x)		=> Some(x)
		}

	def toSeq:Seq[T]	=
		toVector

	def toList:List[T]	=
		this match {
			case Validated.Invalid(x)	=> Nil
			case Validated.Valid(x)		=> List(x)
		}

	def toVector:Vector[T]	=
		this match {
			case Validated.Invalid(x)	=> Vector.empty
			case Validated.Valid(x)		=> Vector(x)
		}

	//------------------------------------------------------------------------------

	def toEitherT[F[_],EE>:E,TT>:T](using F:Applicative[F]):EitherT[F,EE,TT]	=
		EitherT fromEither toEither
}
