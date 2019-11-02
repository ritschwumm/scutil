package scutil.lang

import scutil.lang.tc._

object Optional extends OptionalInstances {
	def partial[S,T](get:PartialFunction[S,T], set:T=>S):Optional[S,T] =
			Optional(get.lift, t => _ => set(t))

	def total[S,T](get:S=>T, set:T=>S):Optional[S,T]	=
			Optional(
				get	= get andThen Some.apply,
				set	= t => _ => set(t)
			)

	def identity[T]:Optional[T,T]	=
			Optional total (Predef.identity, Predef.identity)

	def trivial[T]:Optional[T,Unit]	=
			Optional(
				get	= t 		=> Some(()),
				set	= _ => t	=> t
			)

	def void[S,T]:Optional[S,T]	=
			Optional(
				get	= t 		=> None,
				set	= _ => s	=> s
			)

	def always[T]:Optional[Option[T],T]	=
			Optional(Predef.identity, x => t => Some(x))

	def filtered[T](pred:T=>Boolean):Optional[T,T]	=
			Optional(
				s => if (pred(s)) Some(s) else None,
				x => _ => x
			)

	def codiag[T]:Optional[Either[T,T],T]	=
			identity[T] sum identity[T]

	def fromLensToOption[S,T](lens:Lens[S,Option[T]]):Optional[S,T]	=
			Optional(
				s => lens get s,
				t => s => if (lens.get(s).isDefined) lens.set(Some(t))(s) else s
			)
}

final case class Optional[S,T](get:S=>Option[T], set:T=>S=>S) {
	def setThe(s:S, t:T):S	= set(t)(s)

	//------------------------------------------------------------------------------

	def mod(func:Endo[T]):Endo[S]	=
			s	=> {
				get(s)
				.map		{ t => set(func(t))(s) }
				.getOrElse	(s)
			}
	def modThe(s:S, func:Endo[T]):S	= mod(func) apply s

	def modF[F[_]](func:FEndo[F,T])(implicit F:Applicative[F]):FEndo[F,S]	= s	=> modOptF(func) apply s getOrElse (F pure s)
	def modTheF[F[_]](s:S, func:FEndo[F,T])(implicit F:Applicative[F]):F[S]	= modF(func) apply s

	//------------------------------------------------------------------------------

	def setOpt(t:T):S=>Option[S]	=
			s =>
			if (get(s).isDefined)	Some(set(t)(s))
			else					None

	def setTheOpt(s:S, t:T):Option[S]	= setOpt(t)(s)

	//------------------------------------------------------------------------------

	def modOpt(func:Endo[T]):PEndo[S]	=
			s	=> {
				get(s) map { t =>
					set(func(t))(s)
				}
			}
	def modTheOpt(s:S, func:Endo[T]):Option[S]	= modOpt(func) apply s

	def modOptF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):S=>Option[F[S]]	=
			s	=> {
				get(s) map { t =>
					val ft:F[T] = func(t)
					(F map ft) { t => set(t)(s) }
				}
			}
	def modTheOptF[F[_]](s:S, func:FEndo[F,T])(implicit F:Functor[F]):Option[F[S]]	= modOptF(func) apply s

	//------------------------------------------------------------------------------

	def embedState[U](state:State[T,U]):State[S,Option[U]]	=
			State { s =>
				get(s)
				.map { t1 =>
					val (t2, u)	= state run t1
					set(t2)(s) -> (Some(u):Option[U])
				}
				.getOrElse (s -> None)
			}

	def getState:State[S,Option[T]]	=
			embedState(State.get)

	def setState(it:T):State[S,Option[Unit]]	=
			embedState(State set it)

	def setStateSuccess(it:T):State[S,Boolean]	=
			setState(it) map (_.isDefined)

	def setOldState(it:T):State[S,Option[T]]	=
			embedState(State setOld it)

	def modState(func:T=>T):State[S,Option[Unit]]	=
			embedState(State mod func)

	def modStateSuccess(func:T=>T):State[S,Boolean]	=
			embedState(State mod func) map (_.isDefined)

	def modOldState(func:T=>T):State[S,Option[T]]	=
			embedState(State modOld func)

	// NOTE no transformState

	//------------------------------------------------------------------------------

	def embedStateT[F[_],U](state:StateT[F,T,U])(implicit F:Applicative[F]):StateT[F,S,Option[U]]	=
			StateT { s =>
				get(s)
				.map { t1 =>
					val ftu	= state run t1
					(F map ftu) { case (t2, u) =>
						set(t2)(s) -> (Some(u):Option[U])
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

	def modStateT[F[_]:Applicative](func:Endo[T]):StateT[F,S,Option[Unit]]	=
			embedStateT(StateT mod func)

	def modOldStateT[F[_]:Applicative](func:Endo[T]):StateT[F,S,Option[T]]	=
			embedStateT(StateT modOld func)

	//------------------------------------------------------------------------------

	def embedStateOpt[U](state:State[T,U]):StateT[Option,S,U]	=
			StateT { (s:S) =>
				get(s) map { t1 =>
					val (t2, u)	= state run t1
					set(t2)(s) -> u
				}
			}

	//------------------------------------------------------------------------------

	def orElse(that:Optional[S,T]):Optional[S,T]	=
			Optional(
				get	= s	=> (this get s) orElse (that get s),
				set	= set
			)

	/** filter the source value */
	def filterBefore(pred:Predicate[S]):Optional[S,T]	=
			Optional(
				get	= s	=> if (pred(s)) get(s) else None,
				set	= set
			)

	/** filter the target value */
	def filterAfter(pred:Predicate[T]):Optional[S,T]	=
			Optional(
				get	= s	=> get(s) filter pred,
				set	= set
			)

	//------------------------------------------------------------------------------

	/** symbolic alias for andThen */
	def >=>[U](that:Optional[T,U]):Optional[S,U]	=
			this andThen that

	/** symbolic alias for compose */
	def <=<[R](that:Optional[R,S]):Optional[R,T]	=
			this compose that

	def compose[R](that:Optional[R,S]):Optional[R,T]	=
			that andThen this

	def andThen[U](that:Optional[T,U]):Optional[S,U]	=
			Optional(
				get	= s => this get s flatMap that.get,
				set	= (u:U) => (s:S) => {
					get(s)
					.map { t =>
						this set (that set u apply t) apply s
					}
					.getOrElse(s)
				}
			)

	//------------------------------------------------------------------------------

	// TODO optics does this mean we have an Applicative instance?
	def zip[U](that:Optional[S,U]):Optional[S,(T,U)]	=
			Optional(
				get	= s			=> (this get s) zip (that get s),
				set	= tu => s	=> that set tu._2 apply (this set tu._1 apply s)
			)

	def sum[SS](that:Optional[SS,T]):Optional[Either[S,SS],T]	=
			Optional(
				get	=
					_ match {
						case Left(s)	=> this get s
						case Right(ss)	=> that get ss
					},
				set	= t =>
					_ match {
						case Left(s)	=> Left(this set t apply s)
						case Right(ss)	=> Right(that set t apply ss)
					}
			)

	def product[SS,TT](that:Optional[SS,TT]):Optional[(S,SS),(T,TT)]	=
			Optional(
				get	= sss			=> (this get sss._1) zip (that get sss._2),
				set	= ttt => sss	=> (this set ttt._1 apply sss._1, that set ttt._2 apply sss._2)
			)
}

trait OptionalInstances {
	// TODO optics is this lawful?
	implicit def OptionalSemigroup[S,T]:Semigroup[Optional[S,T]]	=
			Semigroup instance (_ orElse _)
}

