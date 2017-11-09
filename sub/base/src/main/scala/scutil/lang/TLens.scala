package scutil.lang

import scutil.lang.tc._

object TLens {
	def create[S,T](get:S=>T, put:(S,T)=>S):TLens[S,T]	=
			TLens { s =>
				Store[S,T](
					get(s),
					put(s,_)
				)
			}
			
	def identity[T]:TLens[T,T]	=
			TLens(Store.identity)
		
	def trivial[T]:TLens[T,Unit]	=
			TLens(Store.trivial)
	
	def codiag[T]:TLens[Either[T,T],T]	=
			identity[T] sum identity[T]
}

/** functional reference to a part of product type, aka Lens' */
final case class TLens[S,T](on:S=>Store[S,T]) {
	def get(s:S):T	= on(s).get
	def getter:S=>T	= get(_)
	
	def put(s:S, t:T):S		= on(s) put t
	def putter(t:T):Endo[S]	= put(_, t)
	
	def modify(s:S, func:Endo[T]):S		= on(s) modify func
	def modifier(func:Endo[T]):Endo[S]	= modify(_, func)
	
	def modifyF[F[_]:Functor](s:S, func:FEndo[F,T]):F[S]	= modifierF(func) apply s
	// van laarhoven form
	def modifierF[F[_]:Functor](func:FEndo[F,T]):FEndo[F,S]	= on(_) modifyF func
		
	def modifyState[X](s:S, func:State[T,X]):(S,X)	= on(s) modifyState func
	def modifierState[X](func:State[T,X]):S=>(S,X)	= modifyState(_, func)
	
	def modifyStateT[F[_]:Functor,X](s:S, func:StateT[F,T,X]):F[(S,X)]		= on(s) modifyStateT func
	def modifierStateT[F[_]:Functor,X](func:StateT[F,T,X]):StateT[F,S,X]	= StateT { s => on(s) modifyStateT func }
		
	//------------------------------------------------------------------------------
	
	def embedState[U](state:State[T,U]):State[S,U]	=
			state inside this
			// State { on(_) modifyState state }
		
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
	
	def embedStateT[F[_]:Functor,U](state:StateT[F,T,U]):StateT[F,S,U]	=
			state inside this
		
	def getStateT[F[_]:Applicative]:StateT[F,S,T]	=
			embedStateT(StateT.get)
		
	def setStateT[F[_]:Applicative](it:T):StateT[F,S,Unit]	=
			embedStateT(StateT set it)
		
	def modStateT[F[_]:Applicative](func:T=>T):StateT[F,S,Unit]	=
			embedStateT(StateT mod func)
		
	def transformStateT[F[_]:Functor]:StateT[F,T,?] ~> StateT[F,S,?]	=
			new (StateT[F,T,?] ~> StateT[F,S,?]) {
				def apply[X](it:StateT[F,T,X]):StateT[F,S,X]	=
						embedStateT(it)
			}
		
	//------------------------------------------------------------------------------
	
	/** symbolic alias for andThen */
	def >=>[U](that:TLens[T,U]):TLens[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:TLens[R,S]):TLens[R,T]	=
			this compose that
		
	def compose[R](that:TLens[R,S]):TLens[R,T]	=
			that andThen this
		
	def andThen[U](that:TLens[T,U]):TLens[S,U]	=
			TLens { s =>
				val thisStore:Store[S,T]	= this on s
				val thatStore:Store[T,U]	= that on thisStore.get
				Store[S,U](
					thatStore.get,
					thatStore.put andThen thisStore.put
				)
			}
			
	def andThenBijection[U](that:Bijection[T,U]):TLens[S,U]	=
			this >=> that.toTLens
			
	def andThenPLens[U](that:PLens[T,U]):PLens[S,U]	=
			toPLens >=> that
			
	def over[R](store:Store[R,S]):Store[R,T]	=
			this on store.get compose store
		
	def zip[U](that:TLens[S,U]):TLens[S,(T,U)]	=
			TLens { s =>
				Store[S,(T,U)](
					((this on s).get, (that on s).get),
					{ case (t,u)	=> that on (this on s put t) put u }
				)
			}
			
	// |||
	def sum[SS](that:TLens[SS,T]):TLens[Either[S,SS],T]	=
			TLens {
				_ match {
					case Left(s)	=>
						val store	= this on s
						Store[Either[S,SS],T](
							store.get,
							it => Left(store put it)
						)
					case Right(ss)	=>
						val store	= that on ss
						Store[Either[S,SS],T](
							store.get,
							it => Right(store put it)
						)
				}
			}
			
	// ***
	def product[SS,TT](that:TLens[SS,TT]):TLens[(S,SS),(T,TT)]	=
			TLens { case (s,ss)	=>
				val thisStore	= this on s
				val thatStore	= that on ss
				Store[(S,SS),(T,TT)](
					(thisStore.get, thatStore.get),
					{ case (t,tt) => (thisStore put t, thatStore put tt) }
				)
			}
			
	def toPLens:PLens[S,T]	=
			PLens(on andThen Some.apply)
}
