package scutil.lang

import scutil.lang.tc._

object Bijection {
	def identity[T]:Bijection[T,T]	=
			Bijection(Predef.identity, Predef.identity)
		
	val Gen	= BijectionGen
}

final case class Bijection[S,T](get:S=>T, put:T=>S) {
	// can be used as scala function and extractor
	def apply(t:T):S			= put(t)
	def unapply(s:S):Option[T]	= Some(get(s))
	
	//------------------------------------------------------------------------------
	
	@deprecated("0.127.0", "use get")
	def write	= get
	@deprecated("0.127.0", "use put")
	def read	= put
	
	def mod(func:Endo[T]):Endo[S]	= s => put(func(get(s)))
	def modThe(s:S, func:Endo[T]):S	= mod(func)(s)
	
	def modF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):FEndo[F,S]	= s => (F map func(get(s)))(put)
	def modTheF[F[_]:Functor](s:S, func:FEndo[F,T]):F[S]				= modF(func) apply s
	
	@deprecated("0.127.0", "use modThe")
	def modify(s:S, func:Endo[T]):S		= modThe(s, func)
	@deprecated("0.127.0", "use mod")
	def modifier(func:Endo[T]):Endo[S]	= mod(func)
		
	@deprecated("0.127.0", "use modTheF")
	def modifyF[F[_]](s:S, func:FEndo[F,T])(implicit F:Functor[F]):F[S]	= modTheF(s, func)
	@deprecated("0.127.0", "use modF")
	def modifierF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):S=>F[S]	= modF(func)
	
	//------------------------------------------------------------------------------
	
	def embedState[U](state:State[T,U]):State[S,U]	=
			State { s =>
				val (t,u)	= state run get(s)
				(put(t), u)
			}
			
	def embedStateT[F[_],U](state:StateT[F,T,U])(implicit F:Functor[F]):StateT[F,S,U]	=
			StateT { s =>
				val ftu:F[(T,U)]	= state run get(s)
				(F map ftu) { case (t,u) =>
					(put(t), u)
				}
			}
	
	//------------------------------------------------------------------------------
	
	def inverse:Bijection[T,S]	=
			Bijection(put, get)
	
	/** symbolic alias for andThen */
	def >=>[U](that:Bijection[T,U]):Bijection[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Bijection[R,S]):Bijection[R,T]	=
			this compose that
		
	def compose[R](that:Bijection[R,S]):Bijection[R,T]	=
			that andThen this
	
	def andThen[U](that:Bijection[T,U]):Bijection[S,U]	=
			Bijection(
				get	= s	=> that get (this get s),
				put	= u	=> this put  (that put  u)
			)
		
	@deprecated("0.127.0", "use this.toPrism >=> that")
	def andThenPrism[U](that:Prism[T,U]):Prism[S,U]	=
			toPrism >=> that
					
	@deprecated("0.127.0", "use this.toPBijection >=> that")
	def andThenPBijection[U](that:PBijection[T,U]):PBijection[S,U]	=
			toPBijection >=> that
					
	@deprecated("0.127.0", "use this.toLens >=> that")
	def andThenLens[U](that:Lens[T,U]):Lens[S,U]	=
			toLens >=> that
			
	@deprecated("0.127.0", "use this.toPLens >=> that")
	def andThenPLens[U](that:PLens[T,U]):PLens[S,U]	=
			toPLens >=> that
		
	def toPrism:Prism[S,T]	=
			Prism total (get, put)
		
	def toPBijection:PBijection[S,T]	=
			PBijection total (get, put)
		
	def toOptional:Optional[S,T]	=
			Optional total (get, put)
					
	def toLens:Lens[S,T]	=
			Lens(
				get	= get,
				put	= (s,t) => put(t)
			)
					
	def toPLens:PLens[S,T]	=
			PLens { s	=>
				Some(Store(get(s), put))
			}
}
