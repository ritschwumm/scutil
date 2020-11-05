package scutil.lang

import scutil.lang.tc._

object Prism {
	def partial[S,T](get:PartialFunction[S,T], set:T=>S):Prism[S,T] =
		Prism(get.lift, set)

	def total[S,T](get:S=>T, set:T=>S):Prism[S,T] =
		Prism(get andThen Some.apply, set)

	def identity[T]:Prism[T,T] =
		total[T,T](Predef.identity[T], Predef.identity[T])

	def always[T]:Prism[Option[T],T]	=
		Prism(Predef.identity, Some.apply)

	def trivial[T](value:T):Prism[T,Unit]	=
		Prism(_ => Some(()), _ => value)

	def filtered[T](pred:T=>Boolean):Prism[T,T]	=
		Prism(
			it => if (pred(it)) Some(it) else None,
			Predef.identity
		)

	def some[T]:Prism[Option[T],T]			= Prism(x=>x, Some.apply)

	def left[A,B]:Prism[Either[A,B],A]		= Prism(_.swap.toOption,	Left.apply)
	def right[A,B]:Prism[Either[A,B],B]		= Prism(_.toOption,			Right.apply)

	def bad[A,B]:Prism[Validated[A,B],A]	= Prism(_.badToOption,		Bad.apply)
	def good[A,B]:Prism[Validated[A,B],B]	= Prism(_.toOption,			Good.apply)

	//------------------------------------------------------------------------------

	val Gen	 = PrismGen

	//------------------------------------------------------------------------------
	//## typeclass instances

	// TODO optics is this lawful?
	implicit def PrismSemigroup[S,T]:Semigroup[Prism[S,T]]	=
		Semigroup instance (_ orElse _)
}

/** parser and unparser for some data into a side format, aka Prism' */
final case class Prism[S,T](get:S=>Option[T], set:T=>S) {
	// can be used as scala function and extractor
	def apply(t:T):S			= set(t)
	def unapply(s:S):Option[T]	= get(s)

	//------------------------------------------------------------------------------

	// these fall back to the original value if necessary

	def mod(func:T=>T):S=>S	= s => get(s) map (func andThen set) getOrElse s
	def modThe(s:S, func:T=>T):S	= mod(func)(s)

	def modF[F[_]](func:T=>F[T])(implicit F:Applicative[F]):S=>F[S]	= s	=> modOptF(func) apply s getOrElse (F pure s)
	def modTheF[F[_]](s:S, func:T=>F[T])(implicit F:Applicative[F]):F[S]	= modF(func) apply s

	// TODO optics this could be renamed to set, set is actually reverseGet in monocle (?)
	def setMatching(value:T):S=>S	=
		//mod(_ => value)
		s => {
			get(s) match {
				case Some(_)	=> set(value)
				case None		=> s
			}
		}

	//------------------------------------------------------------------------------

	def modOpt(func:T=>T):S=>Option[S]		= s => get(s) map (func andThen set)
	def modTheOpt(s:S, func:T=>T):Option[S]	= modOpt(func) apply s

	def modOptF[F[_]](func:T=>F[T])(implicit F:Functor[F]):S=>Option[F[S]]	=
		s	=> {
			get(s) map { t =>
				(F map func(t)) { ss =>
					set(ss)
				}
			}
		}
	def modTheOptF[F[_]](s:S, func:T=>F[T])(implicit F:Functor[F]):Option[F[S]]	=
		modOptF(func) apply s

	//------------------------------------------------------------------------------

	def embedState[U](state:State[T,U]):State[S,Option[U]]	=
		State { s =>
			get(s)
			.map { t1 =>
				val (t2,u)	= state run t1
				 set(t2) -> (Some(u):Option[U])
			}
			.getOrElse	(s -> None)
		}

	def getState:State[S,Option[T]]	=
		embedState(State.get)

	def setState(it:T):State[S,Option[Unit]]	=
		embedState(State set it)

	def setOldState(it:T):State[S,Option[T]]	=
		embedState(State setOld it)

	def modState(func:T=>T):State[S,Option[Unit]]	=
		embedState(State mod func)

	def modOldState(func:T=>T):State[S,Option[T]]	=
		embedState(State modOld func)

	//------------------------------------------------------------------------------

	def embedStateT[F[_],U](state:StateT[F,T,U])(implicit F:Applicative[F]):StateT[F,S,Option[U]]	=
		StateT { s =>
			get(s)
			.map { t1 =>
				val ftu	= state run t1
				(F map ftu) { case (t2, u) =>
					set(t2) -> (Some(u):Option[U])
				}
			}
			.getOrElse (
				F pure (s -> (None:Option[U]))
			)
		}

	def getStateT[F[_]:Applicative]:StateT[F,S,Option[T]]	=
		embedStateT(StateT.get)

	def setStateT[F[_]:Applicative](it:T):StateT[F,S,Option[Unit]]	=
		embedStateT(StateT set it)

	def setOldStateT[F[_]:Applicative](it:T):StateT[F,S,Option[T]]	=
		embedStateT(StateT setOld it)

	def modStateT[F[_]:Applicative](func:T=>T):StateT[F,S,Option[Unit]]	=
		embedStateT(StateT mod func)

	def modOldStateT[F[_]:Applicative](func:T=>T):StateT[F,S,Option[T]]	=
		embedStateT(StateT modOld func)

	//------------------------------------------------------------------------------

	def embedStateOpt[U](state:State[T,U]):StateT[Option,S,U]	=
		StateT { (s:S) =>
			get(s) map { t1 =>
				val (t2, u)	= state run t1
				set(t2) -> u
			}
		}

	//------------------------------------------------------------------------------

	def orElse(that:Prism[S,T]):Prism[S,T]	=
		Prism(
			get	= s	=> (this get s) orElse (that get s),
			set	= set
		)

	/** filter the source value */
	def filterBefore(pred:Predicate[S]):Prism[S,T]	=
		Prism(
			get	= s	=> if (pred(s)) get(s) else None,
			set	= set
		)

	/** filter the target value */
	def filterAfter(pred:Predicate[T]):Prism[S,T]	=
		Prism(
			get	= s	=> get(s) filter pred,
			set	= set
		)

	//------------------------------------------------------------------------------

	/** symbolic alias for andThen */
	def >=>[U](that:Prism[T,U]):Prism[S,U]	=
		this andThen that

	/** symbolic alias for compose */
	def <=<[R](that:Prism[R,S]):Prism[R,T]	=
		this compose that

	def compose[R](that:Prism[R,S]):Prism[R,T]	=
		that andThen this

	def andThen[U](that:Prism[T,U]):Prism[S,U]	=
		Prism(
			get	= s	=> this get s flatMap that.get,
			set	= t	=> this set (that set t)
		)

	//------------------------------------------------------------------------------

	// impossible
	// def zip[U](that:Prism[S,U]):Prism[S,(T,U)]
	// def sum[SS](that:Prism[SS,T]):Prism[Either[S,SS],T]

	// ***
	def product[SS,TT](that:Prism[SS,TT]):Prism[(S,SS),(T,TT)]	=
		Prism(
			get	= sss 	=> (this get sss._1) zip (that get sss._2),
			set	= ttt	=> (this set ttt._1, that set ttt._2)
		)

	//------------------------------------------------------------------------------

	def toPBijection:PBijection[S,T]	=
		PBijection(
			get	= get,
			set	= t => Some(set(t))
		)

	def toOptional:Optional[S,T]	=
		Optional(
			get	= get,
			set	= setMatching
		)

	def writeExtractor:Extractor[S,T]	=
		Extractor(get)

	// TODO optics this is questionable
	def toBijection(func:S=>T):Bijection[S,T]	=
		Bijection(
			get	= s => get(s) getOrElse func(s),
			set	= set
		)

	// TODO optics this is questionable
	def toBijectionWith(default: =>T):Bijection[S,T]	=
		toBijection(constant(default))
}
