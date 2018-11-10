package scutil.lang

import scala.collection.generic.CanBuildFrom

import scutil.lang.tc._

object Validated extends ValidatedInstances {
	def good[E,T](value:T):Validated[E,T]	= Good(value)
	def bad[E,T](problems:E):Validated[E,T]	= Bad(problems)

	//------------------------------------------------------------------------------

	def switch[E,T](ok:Boolean, problems: =>E, value: =>T):Validated[E,T]	=
			if (ok)	good(value)
			else	bad(problems)

	implicit class MergeableValidated[T](peer:Validated[T,T]) {
		def merge:T	=
				peer match {
					case Bad(x)		=> x
					case Good(x)	=> x
				}
	}
}

sealed trait Validated[+E,+T] {
	def cata[X](bad:E=>X, good:T=>X):X	=
			this match {
				case Good(x)	=> good(x)
				case Bad(x)		=> bad(x)
			}

	//------------------------------------------------------------------------------

	def isGood:Boolean	=
			this match {
				case Bad(x)		=> false
				case Good(x)	=> true
			}

	def isBad:Boolean	=
			!isGood

	//------------------------------------------------------------------------------

	def exists(pred:Predicate[T]):Boolean	=
			this match {
				case Bad(x)		=> false
				case Good(x)	=> pred(x)
			}

	def forall(pred:Predicate[T]):Boolean	=
			this match {
				case Bad(x)		=> true
				case Good(x)	=> pred(x)
			}

	//------------------------------------------------------------------------------

	def iterator:Iterator[T]	=
			this match {
				case Bad(x)		=> Iterator.empty
				case Good(x)	=> Iterator single x
			}

	def foreach(effect:Effect[T]):Unit	=
			this match {
				case Bad(x)		=> ()
				case Good(x)	=> effect(x)
			}

	def map[U](func:T=>U):Validated[E,U]	=
			this match {
				case Bad(x)		=> Bad(x)
				case Good(x)	=> Good(func(x))
			}

	def flatMap[EE>:E,U](func:T=>Validated[EE,U]):Validated[EE,U]	=
			this match {
				case Bad(x)		=> Bad(x)
				case Good(x)	=> func(x)
			}

	def flatten[EE>:E,U](implicit ev:T=>Validated[EE,U]):Validated[EE,U]	=
			flatMap(ev)

	/** function effect first */
	def ap[EE>:E:Semigroup,U,V](that:Validated[EE,U])(implicit ev:T=>U=>V):Validated[EE,V]	=
			that pa (this map ev)

	/** function effect first */
	def pa[EE>:E:Semigroup,U](that:Validated[EE,T=>U]):Validated[EE,U]	=
			(that zip this) map { case (t2u, t) => t2u(t) }

	def zip[EE>:E:Semigroup,U](that:Validated[EE,U]):Validated[EE,(T,U)]	=
			(this zipWith that)((_,_))

	def zipWith[EE>:E,U,V](that:Validated[EE,U])(func:(T,U)=>V)(implicit cc:Semigroup[EE]):Validated[EE,V]	=
			(this, that) match {
				case (Bad(a),	Good(_))	=> Bad(a)
				case (Good(_),	Bad(b))		=> Bad(b)
				case (Bad(a),	Bad(b))		=> Bad(cc concat (a, b))
				case (Good(a),	Good(b))	=> Good(func(a, b))
			}

	/** handy replacement for tried.toISeq.flatten abusing CanBuildFrom as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:T=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			// toOption.flattenMany
			this map ev match {
				case Bad(_)		=> cbf().result
				case Good(cc)	=> cc
			}

	//------------------------------------------------------------------------------

	def swap:Validated[T,E]	=
			this match {
				case Bad(x)		=> Good(x)
				case Good(x)	=> Bad(x)
			}

	def withSwapped[EE,TT](func:Validated[T,E]=>Validated[TT,EE]):Validated[EE,TT]	=
			func(swap).swap

	def bimap[EE,TT](badFunc:E=>EE, goodFunc:T=>TT):Validated[EE,TT]	=
			this match {
				case Bad(x)		=> Bad(badFunc(x))
				case Good(x)	=> Good(goodFunc(x))
			}

	//------------------------------------------------------------------------------

	def badMap[EE](func:E=>EE):Validated[EE,T]	=
			this match {
				case Bad(x)		=> Bad(func(x))
				case Good(x)	=> Good(x)
			}

	def badFlatMap[EE,TT>:T](func:E=>Validated[EE,TT]):Validated[EE,TT]	=
			this match {
				case Bad(x)		=> func(x)
				case Good(x)	=> Good(x)
			}

	def badFlatten[EE,TT>:T](implicit ev:E=>Validated[EE,TT]):Validated[EE,TT]	=
			badFlatMap(ev)

	def badToOption:Option[E]	=
			this match {
				case Bad(x)		=> Some(x)
				case Good(x)	=> None
			}

	//------------------------------------------------------------------------------

	def orElse[EE>:E,TT>:T](that:Validated[EE,TT])(implicit cc:Semigroup[EE]):Validated[EE,TT]	=
			(this, that) match {
				case (Bad(a),	Bad(b))		=> Bad(cc concat (a, b))
				case (Good(a),	_)			=> Good(a)
				case (_,		Good(b))	=> Good(b)
			}

	def getOrElse[TT>:T](that: =>TT):TT	=
			this match {
				case Bad(x)		=> that
				case Good(x)	=> x
			}

	def getOrRescue[TT>:T](func:E=>TT):TT	=
			this match {
				case Bad(x)		=> func(x)
				case Good(x)	=> x
			}

	def getOrError(s: =>String):T	=
			getOrElse(sys error s)

	def getOrThrow(func:E=>Throwable):T	=
			this match {
				case Bad(x)		=> throw func(x)
				case Good(x)	=> x
			}

	//------------------------------------------------------------------------------

	def rescue[TT>:T](func:PFunction[E,TT]):Validated[E,TT]	=
			this match {
				case Bad(x)		=> func(x) map Good.apply getOrElse Bad(x)
				case Good(x)	=> Good(x)
			}

	def reject[EE>:E](func:PFunction[T,EE]):Validated[EE,T]	=
			this match {
				case Bad(x)		=> Bad(x)
				case Good(x)	=> func(x) map Bad.apply getOrElse Good(x)
			}

	def winByOr[EE>:E](func:Predicate[T], bad: =>EE):Validated[EE,T]	=
			this match {
				case Bad(x)		=> Bad(x)
				case Good(x)	=> if (func(x)) Good(x) else Bad(bad)
			}

	def winNotByOr[EE>:E](func:Predicate[T], bad: =>EE):Validated[EE,T]	=
			this match {
				case Bad(x)		=> Bad(x)
				case Good(x)	=> if (!func(x)) Good(x) else Bad(bad)
			}

	def collapseOr[EE>:E,TT](func:PFunction[T,TT], bad: =>EE):Validated[EE,TT]	=
			this match {
				case Bad(x)		=> Bad(x)
				case Good(x)	=> func(x) map Good.apply getOrElse Bad(bad)
			}

	def collectOr[EE>:E,TT](func:PartialFunction[T,TT], bad: =>EE):Validated[EE,TT]	=
			collapseOr(func.lift, bad)

	//------------------------------------------------------------------------------

	def goodEffect(effect:Effect[T]):this.type		= {
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
				case Bad(x)		=> Left(x)
				case Good(x)	=> Right(x)
			}

	def toOption:Option[T]	=
			this match {
				case Bad(x)		=> None
				case Good(x)	=> Some(x)
			}

	def toISeq:ISeq[T]	=
			toVector

	def toList:List[T]	=
			this match {
				case Bad(x)		=> Nil
				case Good(x)	=> List(x)
			}

	def toVector:Vector[T]	=
			this match {
				case Bad(x)		=> Vector.empty
				case Good(x)	=> Vector(x)
			}

	//------------------------------------------------------------------------------

	def toEitherT[F[_],EE>:E,TT>:T](implicit F:Applicative[F]):EitherT[F,EE,TT]	=
			EitherT fromEither toEither
}

final case class Bad[E](problems:E)	extends Validated[E,Nothing]
final case class Good[T](value:T)	extends Validated[Nothing,T]

trait ValidatedInstances {
	implicit def ValidatedApplicative[S:Semigroup]:Applicative[Validated[S,?]]	=
			new Applicative[Validated[S,?]] {
				override def pure[A](it:A):Validated[S,A]										= Validated good it
				override def ap[A,B](it:Validated[S,A])(func:Validated[S,A=>B]):Validated[S,B]	= it pa func
			}

	implicit def ValidatedSemigroup[S:Semigroup,T]:Semigroup[Validated[S,T]]	=
			Semigroup instance (_ orElse _)
}
