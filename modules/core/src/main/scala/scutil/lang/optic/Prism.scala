package scutil.lang

import scala.reflect.*

import scutil.lang.tc.*

object Prism {
	val Gen	= PrismGen

	//-----------------------------------------------------------------------------

	def partial[S,T](get:PartialFunction[S,T], set:T=>S):Prism[S,T] =
		Prism(get.lift, set)

	def total[S,T](get:S=>T, set:T=>S):Prism[S,T] =
		Prism(get `andThen` Some.apply, set)

	def identity[T]:Prism[T,T] =
		total[T,T](Predef.identity[T], Predef.identity[T])

	def always[T]:Prism[Option[T],T]	=
		Prism(Predef.identity, Some.apply)

	def trivial[T](value:T):Prism[T,Unit]	=
		Prism(_ => Some(()), _ => value)

	def filtered[T](pred:T=>Boolean):Prism[T,T]	=
		Prism(
			it => if pred(it) then Some(it) else None,
			Predef.identity
		)

	//------------------------------------------------------------------------------

	def subType[S,T<:S](using tt:TypeTest[S,T]):Prism[S,T]	=
		Prism(tt.unapply, Predef.identity)

	/**
	example:

	val P = Prism.subTypes[MyEnum]
	val p:Prism[MyEnum, MyEnumVairant] = P[MyEnumVariant]
	*/
	def subTypes[S]:[T <: S] => (TypeTest[S, T]) ?=> Prism[S, T]	=
		[T<:S] => (tt:TypeTest[S, T]) ?=> subType[S,T]
}

/** parser and unparser for some data into a side format, aka Prism' */
final case class Prism[S,T](get:S=>Option[T], set:T=>S) {
	// can be used as scala function and extractor
	def apply(t:T):S			= set(t)
	def unapply(s:S):Option[T]	= get(s)

	//------------------------------------------------------------------------------

	// these fall back to the original value if necessary

	def mod(func:T=>T):S=>S	= s => get(s).map(func.andThen(set)).getOrElse(s)

	def modF[F[_]:Applicative as F](func:T=>F[T]):S=>F[S]	= s	=> modOptF(func).apply(s).getOrElse(F.pure(s))

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

	def modOpt(func:T=>T):S=>Option[S]		= s => get(s).map(func.andThen(set))

	def modOptF[F[_]:Functor as F](func:T=>F[T]):S=>Option[F[S]]	=
		s	=> {
			get(s).map { t =>
				F.map(func(t)) { ss =>
					set(ss)
				}
			}
		}

	//------------------------------------------------------------------------------

	def embedState[U](state:State[T,U]):State[S,Option[U]]	=
		State { s =>
			get(s)
			.map { t1 =>
				val (t2, u)	= state.run(t1)
				set(t2) -> Some(u)
			}
			.getOrElse	(s -> None)
		}

	def getState:State[S,Option[T]]	=
		embedState(State.get)

	def setState(it:T):State[S,Option[Unit]]	=
		embedState(State.set(it))

	def setOldState(it:T):State[S,Option[T]]	=
		embedState(State.setOld(it))

	def modState(func:T=>T):State[S,Option[Unit]]	=
		embedState(State.mod(func))

	def modOldState(func:T=>T):State[S,Option[T]]	=
		embedState(State.modOld(func))

	//------------------------------------------------------------------------------

	def embedStateT[F[_]:Applicative as F,U](state:StateT[F,T,U]):StateT[F,S,Option[U]]	=
		StateT { s =>
			get(s)
			.map { t1 =>
				val ftu	= state.run(t1)
				F.map(ftu) { (t2, u) =>
					set(t2) -> Some(u)
				}
			}
			.getOrElse (
				F.pure(s -> None)
			)
		}

	def getStateT[F[_]:Applicative]:StateT[F,S,Option[T]]	=
		embedStateT(StateT.get)

	def setStateT[F[_]:Applicative](it:T):StateT[F,S,Option[Unit]]	=
		embedStateT(StateT.set(it))

	def setOldStateT[F[_]:Applicative](it:T):StateT[F,S,Option[T]]	=
		embedStateT(StateT.setOld(it))

	def modStateT[F[_]:Applicative](func:T=>T):StateT[F,S,Option[Unit]]	=
		embedStateT(StateT.mod(func))

	def modOldStateT[F[_]:Applicative](func:T=>T):StateT[F,S,Option[T]]	=
		embedStateT(StateT.modOld(func))

	//------------------------------------------------------------------------------

	def embedStateOpt[U](state:State[T,U]):StateT[Option,S,U]	=
		StateT { (s:S) =>
			get(s).map { t1 =>
				val (t2, u)	= state.run(t1)
				set(t2) -> u
			}
		}

	//------------------------------------------------------------------------------

	/** filter the source value */
	def filterBefore(pred:Predicate[S]):Prism[S,T]	=
		Prism(
			get	= s	=> if pred(s) then get(s) else None,
			set	= set
		)

	/** filter the target value */
	def filterAfter(pred:Predicate[T]):Prism[S,T]	=
		Prism(
			get	= s	=> get(s).filter(pred),
			set	= set
		)

	//------------------------------------------------------------------------------

	/** symbolic alias for andThen */
	def >=>[U](that:Prism[T,U]):Prism[S,U]	=
		this.andThen(that)

	/** symbolic alias for compose */
	def <=<[R](that:Prism[R,S]):Prism[R,T]	=
		this.compose(that)

	def compose[R](that:Prism[R,S]):Prism[R,T]	=
		that.andThen(this)

	def andThen[U](that:Prism[T,U]):Prism[S,U]	=
		Prism(
			get	= s	=> this.get(s).flatMap(that.get),
			set	= t	=> this.set(that.set(t))
		)

	//------------------------------------------------------------------------------

	// impossible
	// def zip[U](that:Prism[S,U]):Prism[S,(T,U)]
	// def sum[SS](that:Prism[SS,T]):Prism[Either[S,SS],T]

	// ***
	def product[SS,TT](that:Prism[SS,TT]):Prism[(S,SS),(T,TT)]	=
		Prism(
			get	= sss	=> this.get(sss._1) `zip` that.get(sss._2),
			set	= ttt	=> (this.set(ttt._1), that.set(ttt._2))
		)

	//------------------------------------------------------------------------------

	def writeExtractor:Extractor[S,T]	=
		Extractor(get)

	def toSetter:Setter[S,T]	=
		Setter(mod)

	def toOptional:Optional[S,T]	=
		Optional(
			get	= get,
			set	= setMatching
		)

	// TODO optics this is questionable
	def toBijection(func:S=>T):Bijection[S,T]	=
		Bijection(
			get	= s => get(s).getOrElse(func(s)),
			set	= set
		)

	// TODO optics this is questionable
	def toBijectionWith(default: =>T):Bijection[S,T]	=
		toBijection(constant(default))
}
