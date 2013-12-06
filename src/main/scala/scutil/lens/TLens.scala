package scutil.lens

import scutil.lang._
import scutil.implicits._

object TLens {
	def create[S,T](get:S=>T, put:(S,T)=>S):TLens[S,T]	=
			TLens(s =>
				Store[S,T](
					get(s),
					put(s,_)
				)
			)
			
	def identity[T]:TLens[T,T]	= 
			TLens(Store.identity)
		
	def trivial[T]:TLens[T,Unit]	=
			TLens(Store.trivial)
	
	def bijection[S,T](bijection:Bijection[S,T]):TLens[S,T]	= 
			TLens { s	=>
				Store(bijection write s, bijection.read)
			}
		
	def codiag[T]:TLens[Either[T,T],T]	=
			identity[T] sum identity[T]
}

final case class TLens[S,T](on:S=>Store[S,T]) {
	def get(s:S):T	= on(s).get
	
	def put(s:S, t:T):S		= on(s) put t
	def putter(t:T):Endo[S]	= put(_, t)
	
	def modify(s:S, func:Endo[T]):S		= on(s) modify func
	def modifier(func:Endo[T]):Endo[S]	= modify(_, func)
	
	/** symbolic alias for andThen */
	@inline
	def >=>[U](that:TLens[T,U]):TLens[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	@inline
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
			
	def xmapContainer[R](bijection:Bijection[R,S]):TLens[R,T]	=
			TLens { r =>
				on(bijection write r) map bijection.read
			}
		
	def xmapContainerInverse[R](bijection:Bijection[S,R]):TLens[R,T]	=
			xmapContainer(bijection.inverse)
		
	def xmapValue[U](bijection:Bijection[T,U]):TLens[S,U]	=
			TLens { s =>
				on(s) xmapValue bijection
			}
			
	def xmapValueInverse[U](bijection:Bijection[U,T]):TLens[S,U]	=
			xmapValue(bijection.inverse)
			
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
