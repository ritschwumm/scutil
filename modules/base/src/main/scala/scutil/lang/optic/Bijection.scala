package scutil.lang

import scutil.lang.tc._

object Bijection {
	def identity[T]:Bijection[T,T]	=
			Bijection(Predef.identity, Predef.identity)
		
	val Gen	= BijectionGen
}

final case class Bijection[S,T](get:S=>T, set:T=>S) {
	@deprecated("use set", "0.130.0")
	def put(t:T):S	= set(t)
	
	// can be used as scala function and extractor
	def apply(t:T):S			= set(t)
	def unapply(s:S):Option[T]	= Some(get(s))
	
	//------------------------------------------------------------------------------
	
	def mod(func:Endo[T]):Endo[S]	= s => set(func(get(s)))
	def modThe(s:S, func:Endo[T]):S	= mod(func)(s)
	
	def modF[F[_]](func:FEndo[F,T])(implicit F:Functor[F]):FEndo[F,S]	= s => (F map func(get(s)))(set)
	def modTheF[F[_]:Functor](s:S, func:FEndo[F,T]):F[S]				= modF(func) apply s
	
	//------------------------------------------------------------------------------
	
	def embedState[U](state:State[T,U]):State[S,U]	=
			State { s =>
				val (t,u)	= state run get(s)
				set(t) -> u
			}
			
	def embedStateT[F[_],U](state:StateT[F,T,U])(implicit F:Functor[F]):StateT[F,S,U]	=
			StateT { s =>
				val ftu:F[(T,U)]	= state run get(s)
				(F map ftu) { case (t,u) =>
					set(t) -> u
				}
			}
	
	//------------------------------------------------------------------------------
	
	def inverse:Bijection[T,S]	=
			Bijection(set, get)
	
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
				set	= u	=> this set  (that set  u)
			)
		
	def toPrism:Prism[S,T]	=
			Prism total (get, set)
		
	def toPBijection:PBijection[S,T]	=
			PBijection total (get, set)
		
	def toOptional:Optional[S,T]	=
			Optional total (get, set)
					
	def toLens:Lens[S,T]	=
			Lens(
				get	= get,
				set	= t => s => set(t)
			)
					
	def toPLens:PLens[S,T]	=
			PLens { s	=>
				Some(Store(get(s), set))
			}
}