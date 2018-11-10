package scutil.lang

import scutil.lang.tc._

object Lens {
	def identity[T]:Lens[T,T]	=
			// from(Store.identity)
			Lens(t => t, _ => t => t)

	def trivial[T]:Lens[T,Unit]	=
			// from(Store.trivial)
			Lens(_ => (), _ => t => t)

	def codiag[T]:Lens[Either[T,T],T]	=
			identity[T] sum identity[T]

	def fromStoreAt[S,T](func:S=>Store[S,T]):Lens[S,T]	=
			Lens(
				get	= s			=> func(s).get,
				set	= t => s	=> func(s) set t
			)

	val Gen	= LensGen
}

/** functional reference to a part of product type, aka Lens' */
final case class Lens[S,T](get:S=>T, set:T=>S=>S) {
	def setThe(s:S, t:T):S	= set(t)(s)

	//------------------------------------------------------------------------------

	def mod(func:Endo[T]):Endo[S]	= s => set apply func(get(s)) apply s
	def modThe(s:S, func:Endo[T]):S	= mod(func)(s)

	// van laarhoven form
	def modF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):FEndo[F,S]	= s	=> (F map func(get(s))) { t => set(t) apply s }
	def modTheF[F[_]:Functor](s:S, func:FEndo[F,T]):F[S]				= modF(func) apply s

	//------------------------------------------------------------------------------

	def embedState[U](state:State[T,U]):State[S,U]	=
			State { s =>
				val (t,u)	= state run get(s)
				(set(t)(s), u)
			}

	def getState:State[S,T]	=
			embedState(State.get)

	def setState(it:T):State[S,Unit]	=
			embedState(State set it)

	def setOldState(it:T):State[S,T]	=
			embedState(State setOld it)

	def modState(func:T=>T):State[S,Unit]	=
			embedState(State mod func)

	def modOldState(func:T=>T):State[S,T]	=
			embedState(State modOld func)

	def transformState:State[T,?] ~> State[S,?]	=
			new (State[T,?] ~> State[S,?]) {
				def apply[X](it:State[T,X]):State[S,X]	=
						embedState(it)
			}

	//------------------------------------------------------------------------------

	def embedStateT[F[_],U](state:StateT[F,T,U])(implicit F:Functor[F]):StateT[F,S,U]	=
			StateT { s =>
				val ftu:F[(T,U)] = state run get(s)
				(F map ftu) { case (t,u) =>
					(set(t)(s), u)
				}
			}

	def getStateT[F[_]:Applicative]:StateT[F,S,T]	=
			embedStateT(StateT.get)

	def setStateT[F[_]:Applicative](it:T):StateT[F,S,Unit]	=
			embedStateT(StateT set it)

	def setOldStateT[F[_]:Applicative](it:T):StateT[F,S,T]	=
			embedStateT(StateT setOld it)

	def modStateT[F[_]:Applicative](func:Endo[T]):StateT[F,S,Unit]	=
			embedStateT(StateT mod func)

	def modOldStateT[F[_]:Applicative](func:Endo[T]):StateT[F,S,T]	=
			embedStateT(StateT modOld func)

	def transformStateT[F[_]:Functor]:StateT[F,T,?] ~> StateT[F,S,?]	=
			new (StateT[F,T,?] ~> StateT[F,S,?]) {
				def apply[X](it:StateT[F,T,X]):StateT[F,S,X]	=
						embedStateT(it)
			}

	//------------------------------------------------------------------------------

	/** symbolic alias for andThen */
	def >=>[U](that:Lens[T,U]):Lens[S,U]	=
			this andThen that

	/** symbolic alias for compose */
	def <=<[R](that:Lens[R,S]):Lens[R,T]	=
			this compose that

	def compose[R](that:Lens[R,S]):Lens[R,T]	=
			that andThen this

	def andThen[U](that:Lens[T,U]):Lens[S,U]	=
			Lens(
				get	= s 		=> that get (this get s),
				set	= u => s	=> this set (that set u apply (this get s)) apply s
			)

	//------------------------------------------------------------------------------

	// TODO optics does this mean we have an Applicative instance?
	def zip[U](that:Lens[S,U]):Lens[S,(T,U)]	=
			Lens(
				get	= s				=> (this get s, that get s),
				set	= tu	=> s	=> that set tu._2 apply  (this set tu._1 apply s)
			)

	// |||
	def sum[SS](that:Lens[SS,T]):Lens[Either[S,SS],T]	=
			Lens(
				get	= sss	=> sss match {
					case Left(s)	=> this get s
					case Right(ss)	=> that get ss
				},
				set	= t => _ match {
					case Left(s)	=> Left(this set t apply s)
					case Right(ss)	=> Right(that set t apply ss)
				}
			)

	// ***
	def product[SS,TT](that:Lens[SS,TT]):Lens[(S,SS),(T,TT)]	=
			Lens(
				get	= sss 			=> (this get sss._1, that get sss._2),
				set	= ttt => sss	=> (this set ttt._1 apply sss._1, that set ttt._2 apply sss._2)
			)

	//------------------------------------------------------------------------------

	def toOptional:Optional[S,T]	=
			Optional(
				get	= get andThen Some.apply,
				set	= set
			)

	//------------------------------------------------------------------------------

	def on(s:S):Store[S,T]	=
			Store(
				get	= get(s),
				set	= t => set(t)(s)
			)

	def over[R](store:Store[R,S]):Store[R,T]	=
			this on store.get compose store

	def toPLens:PLens[S,T]	=
			PLens(on _ andThen Some.apply)
}
