package scutil.lang

import scutil.lang.tc._

object Lens {
	def identity[T]:Lens[T,T]	=
			// from(Store.identity)
			Lens(t => t, (t, _) => t)
		
	def trivial[T]:Lens[T,Unit]	=
			// from(Store.trivial)
			Lens(_ => (), (t, _) => t)
	
	def codiag[T]:Lens[Either[T,T],T]	=
			identity[T] sum identity[T]
		
	def fromStoreAt[S,T](func:S=>Store[S,T]):Lens[S,T]	=
			Lens(
				get	= s 	=> func(s).get,
				put	= (s,t)	=> func(s) put t
			)
		
	val Gen	= LensGen
}

/** functional reference to a part of product type, aka Lens' */
final case class Lens[S,T](get:S=>T, put:(S,T)=>S) {
	@deprecated("0.127.0", "use get")
	def getter	= get
	
	def putter(t:T):Endo[S]	= put(_, t)
	
	def mod(func:Endo[T]):Endo[S]	= s => put(s, func(get(s)))
	def modThe(s:S, func:Endo[T]):S	= mod(func)(s)
	
	// van laarhoven form
	def modF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):FEndo[F,S]	= s	=> (F map func(get(s))) { t => put(s,t) }
	def modTheF[F[_]:Functor](s:S, func:FEndo[F,T]):F[S]				= modF(func) apply s
	
	@deprecated("0.127.0", "use modThe")
	def modify(s:S, func:Endo[T]):S		= modThe(s, func)
	@deprecated("0.127.0", "use mod")
	def modifier(func:Endo[T]):Endo[S]	= mod(func)
			
	@deprecated("0.127.0", "use modTheF")
	def modifyF[F[_]](s:S, func:FEndo[F,T])(implicit F:Functor[F]):F[S]	= modTheF(s, func)
	@deprecated("0.127.0", "use modF")
	def modifierF[F[_]:Functor](func:FEndo[F,T]):FEndo[F,S]	= modF(func)
	
	//------------------------------------------------------------------------------
	
	def embedState[U](state:State[T,U]):State[S,U]	=
			State { s =>
				val (t,u)	= state run get(s)
				(put(s, t), u)
			}
		
	def getState:State[S,T]	=
			embedState(State.get)
		
	def setState(it:T):State[S,Unit]	=
			embedState(State set it)
		
	def modState(func:T=>T):State[S,Unit]	=
			embedState(State mod func)
		
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
					(put(s, t), u)
				}
			}
		
	def getStateT[F[_]:Applicative]:StateT[F,S,T]	=
			embedStateT(StateT.get)
		
	def setStateT[F[_]:Applicative](it:T):StateT[F,S,Unit]	=
			embedStateT(StateT set it)
		
	def modStateT[F[_]:Applicative](func:Endo[T]):StateT[F,S,Unit]	=
			embedStateT(StateT mod func)
		
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
				put	= (s, u)	=> {
					this put (s, that put (this get s, u))
				}
			)
			
	@deprecated("0.127.0", "use this >=> that.toLens")
	def andThenBijection[U](that:Bijection[T,U]):Lens[S,U]	=
			this >=> that.toLens
			
	@deprecated("0.127.0", "use this.toPLens >=> that")
	def andThenPLens[U](that:PLens[T,U]):PLens[S,U]	=
			toPLens >=> that
		
	//------------------------------------------------------------------------------
			
	// TODO optics does this mean we have an Applicative instance?
	def zip[U](that:Lens[S,U]):Lens[S,(T,U)]	=
			Lens(
				get	= s			=> (this get s, that get s),
				put	= (s, tu)	=> that put (this put (s, tu._1), tu._2)
			)
			
	// |||
	def sum[SS](that:Lens[SS,T]):Lens[Either[S,SS],T]	=
			Lens(
				get	= sss	=> sss match {
					case Left(s)	=> this get s
					case Right(ss)	=> that get ss
				},
				put	= (sss, t)	=> sss match {
					case Left(s)	=> Left(this put (s, t))
					case Right(ss)	=> Right(that put (ss, t))
				}
			)
			
	// ***
	def product[SS,TT](that:Lens[SS,TT]):Lens[(S,SS),(T,TT)]	=
			Lens(
				get	= sss 			=> (this get sss._1, that get sss._2),
				put	= (sss, ttt)	=> (this put (sss._1, ttt._1), that put (sss._2, ttt._2))
			)
			
	//------------------------------------------------------------------------------
		
	def toOptional:Optional[S,T]	=
			Optional(
				get	= get andThen Some.apply,
				put	= put
			)
			
	//------------------------------------------------------------------------------
			
	def on(s:S):Store[S,T]	=
			Store(
				get	= get(s),
				put	= t => put(s, t)
			)
			
	def over[R](store:Store[R,S]):Store[R,T]	=
			this on store.get compose store
		
	def toPLens:PLens[S,T]	=
			PLens(on _ andThen Some.apply)
}
