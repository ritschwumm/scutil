package scutil.lang

import scala.collection.Factory

import scutil.lang.tc._

object Validated {
	def good[E,T](value:T):Validated[E,T]	= Good(value)
	def bad[E,T](problems:E):Validated[E,T]	= Bad(problems)

	//------------------------------------------------------------------------------

	def switch[E,T](ok:Boolean, problems: =>E, value: =>T):Validated[E,T]	=
		if (ok)	good(value)
		else	bad(problems)

	implicit final class MergeableValidated[T](peer:Validated[T,T]) {
		def merge:T	=
			peer match {
				case Bad(x)		=> x
				case Good(x)	=> x
			}
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def ValidatedApplicative[S:Semigroup]:Applicative[Validated[S,*]]	=
		new Applicative[Validated[S,*]] {
			override def pure[A](it:A):Validated[S,A]										= Validated good it
			override def ap[A,B](it:Validated[S,A])(func:Validated[S,A=>B]):Validated[S,B]	= func ap it
		}

	implicit def ValidatedSemigroup[S:Semigroup,T]:Semigroup[Validated[S,T]]	=
		Semigroup instance (_ orElse _)

	//------------------------------------------------------------------------------

	final case class Bad[E](problems:E)	extends Validated[E,Nothing]
	final case class Good[T](value:T)	extends Validated[Nothing,T]
}

sealed trait Validated[+E,+T] {
	def cata[X](bad:E=>X, good:T=>X):X	=
		this match {
			case Validated.Good(x)	=> good(x)
			case Validated.Bad(x)	=> bad(x)
		}

	//------------------------------------------------------------------------------

	def isGood:Boolean	=
		this match {
			case Validated.Bad(x)	=> false
			case Validated.Good(x)	=> true
		}

	def isBad:Boolean	=
		!isGood

	//------------------------------------------------------------------------------

	def exists(pred:Predicate[T]):Boolean	=
		this match {
			case Validated.Bad(x)	=> false
			case Validated.Good(x)	=> pred(x)
		}

	def forall(pred:Predicate[T]):Boolean	=
		this match {
			case Validated.Bad(x)	=> true
			case Validated.Good(x)	=> pred(x)
		}

	//------------------------------------------------------------------------------

	def iterator:Iterator[T]	=
		this match {
			case Validated.Bad(x)	=> Iterator.empty
			case Validated.Good(x)	=> Iterator single x
		}

	def foreach(effect:Effect[T]):Unit	=
		this match {
			case Validated.Bad(x)	=> ()
			case Validated.Good(x)	=> effect(x)
		}

	def map[U](func:T=>U):Validated[E,U]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(x)
			case Validated.Good(x)	=> Validated.good(func(x))
		}

	def flatMap[EE>:E,U](func:T=>Validated[EE,U]):Validated[EE,U]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(x)
			case Validated.Good(x)	=> func(x)
		}

	def flatten[EE>:E,U](implicit ev:T=>Validated[EE,U]):Validated[EE,U]	=
		flatMap(ev)

	/** function effect first */
	def ap[EE>:E:Semigroup,U,V](that:Validated[EE,U])(implicit ev:T=>U=>V):Validated[EE,V]	=
		that pa (this map ev)

	/** function effect first */
	def pa[EE>:E:Semigroup,U](that:Validated[EE,T=>U]):Validated[EE,U]	=
		(that map2 this)((t2u, t) => t2u(t))

	def tuple[EE>:E:Semigroup,U](that:Validated[EE,U]):Validated[EE,(T,U)]	=
		(this map2 that)((_,_))

	def map2[EE>:E,U,V](that:Validated[EE,U])(func:(T,U)=>V)(implicit cc:Semigroup[EE]):Validated[EE,V]	=
		(this, that) match {
			case (Validated.Bad(a),		Validated.Good(_))	=> Validated.Bad(a)
			case (Validated.Good(_),	Validated.Bad(b))	=> Validated.Bad(b)
			case (Validated.Bad(a),		Validated.Bad(b))	=> Validated.Bad(cc.combine(a, b))
			case (Validated.Good(a),	Validated.Good(b))	=> Validated.Good(func(a, b))
		}

	/** handy replacement for tried.toSeq.flatten abusing Factory as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:T=>CC[U], factory:Factory[U,CC[U]]):CC[U]	=
		// toOption.flattenMany
		this map ev match {
			case Validated.Bad(_)	=> factory.newBuilder.result()
			case Validated.Good(cc)	=> cc
		}

	//------------------------------------------------------------------------------

	def swap:Validated[T,E]	=
		this match {
			case Validated.Bad(x)	=> Validated.good(x)
			case Validated.Good(x)	=> Validated.bad(x)
		}

	def withSwapped[EE,TT](func:Validated[T,E]=>Validated[TT,EE]):Validated[EE,TT]	=
		func(swap).swap

	def bimap[EE,TT](badFunc:E=>EE, goodFunc:T=>TT):Validated[EE,TT]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(badFunc(x))
			case Validated.Good(x)	=> Validated.good(goodFunc(x))
		}

	//------------------------------------------------------------------------------

	def badMap[EE](func:E=>EE):Validated[EE,T]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(func(x))
			case Validated.Good(x)	=> Validated.good(x)
		}

	def badFlatMap[EE,TT>:T](func:E=>Validated[EE,TT]):Validated[EE,TT]	=
		this match {
			case Validated.Bad(x)	=> func(x)
			case Validated.Good(x)	=> Validated.good(x)
		}

	def badFlatten[EE,TT>:T](implicit ev:E=>Validated[EE,TT]):Validated[EE,TT]	=
		badFlatMap(ev)

	def badToOption:Option[E]	=
		this match {
			case Validated.Bad(x)	=> Some(x)
			case Validated.Good(x)	=> None
		}

	//------------------------------------------------------------------------------

	def orElse[EE>:E,TT>:T](that:Validated[EE,TT])(implicit cc:Semigroup[EE]):Validated[EE,TT]	=
		(this, that) match {
			case (Validated.Bad(a),		Validated.Bad(b))	=> Validated.bad(cc.combine(a, b))
			case (Validated.Good(a),	_)					=> Validated.good(a)
			case (_,					Validated.Good(b))	=> Validated.good(b)
		}

	def getOrElse[TT>:T](that: =>TT):TT	=
		this match {
			case Validated.Bad(x)	=> that
			case Validated.Good(x)	=> x
		}

	def getOrRescue[TT>:T](func:E=>TT):TT	=
		this match {
			case Validated.Bad(x)	=> func(x)
			case Validated.Good(x)	=> x
		}

	def getOrError(s: =>String):T	=
		getOrElse(sys error s)

	def getOrThrow(func:E=>Throwable):T	=
		this match {
			case Validated.Bad(x)	=> throw func(x)
			case Validated.Good(x)	=> x
		}

	//------------------------------------------------------------------------------

	def rescue[TT>:T](func:E=>Option[TT]):Validated[E,TT]	=
		this match {
			case Validated.Bad(x)	=> func(x) map Validated.good getOrElse Validated.bad(x)
			case Validated.Good(x)	=> Validated.good(x)
		}

	def reject[EE>:E](func:T=>Option[EE]):Validated[EE,T]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(x)
			case Validated.Good(x)	=> func(x) map Validated.bad getOrElse Validated.good(x)
		}

	def winByOr[EE>:E](func:Predicate[T], bad: =>EE):Validated[EE,T]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(x)
			case Validated.Good(x)	=> if (func(x)) Validated.good(x) else Validated.bad(bad)
		}

	def winNotByOr[EE>:E](func:Predicate[T], bad: =>EE):Validated[EE,T]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(x)
			case Validated.Good(x)	=> if (!func(x)) Validated.good(x) else Validated.bad(bad)
		}

	def collapseOr[EE>:E,TT](func:T=>Option[TT], bad: =>EE):Validated[EE,TT]	=
		this match {
			case Validated.Bad(x)	=> Validated.bad(x)
			case Validated.Good(x)	=> func(x) map Validated.good getOrElse Validated.bad(bad)
		}

	def collectOr[EE>:E,TT](func:PartialFunction[T,TT], bad: =>EE):Validated[EE,TT]	=
		collapseOr(func.lift, bad)

	//------------------------------------------------------------------------------

	def goodEffect(effect:Effect[T]):this.type	= {
		this foreach effect
		this
	}

	def badEffect(effect:Effect[E]):this.type	= {
		badToOption foreach effect
		this
	}

	//------------------------------------------------------------------------------

	def toEither:Either[E,T]	=
		this match {
			case Validated.Bad(x)	=> Left(x)
			case Validated.Good(x)	=> Right(x)
		}

	def toOption:Option[T]	=
		this match {
			case Validated.Bad(x)	=> None
			case Validated.Good(x)	=> Some(x)
		}

	def toSeq:Seq[T]	=
		toVector

	def toList:List[T]	=
		this match {
			case Validated.Bad(x)	=> Nil
			case Validated.Good(x)	=> List(x)
		}

	def toVector:Vector[T]	=
		this match {
			case Validated.Bad(x)	=> Vector.empty
			case Validated.Good(x)	=> Vector(x)
		}

	//------------------------------------------------------------------------------

	def toEitherT[F[_],EE>:E,TT>:T](implicit F:Applicative[F]):EitherT[F,EE,TT]	=
		EitherT fromEither toEither
}
