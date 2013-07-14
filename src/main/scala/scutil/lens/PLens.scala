package scutil.lens

import scutil.lang._
import scutil.Implicits._

object PLens {
	def identity[T]:PLens[T,T]	= 
			PLens { t =>
				Some(Store identity t)
			}
		
	def trivial[T]:PLens[T,Unit]	=
			PLens { t =>
				Some(Store trivial t)
			}
		
	def bijection[S,T](bijection:Bijection[S,T]):PLens[S,T]	= 
			PLens { s	=>
				Some(Store(bijection write s, bijection.read))
			}
		
	def codiag[T]:PLens[Either[T,T],T]	=
			identity[T] sum identity[T]
}

case class PLens[S,T](on:S=>Option[Store[S,T]]) {
	def get(s:S):Option[T]	= on(s) map { _.get }
	
	def put(s:S, t:T):Option[S]	= on(s) map { _ put t }
	def putter(t:T):PEndo[S]	= put(_, t)
	
	def modify(s:S, func:Endo[T]):Option[S]	= on(s) map { _ mod func }
	def modifier(func:Endo[T]):PEndo[S]		= modify(_, func)
	
	def compose[R](that:PLens[R,S]):PLens[R,T]	=
			that andThen this
		
	def andThen[U](that:PLens[T,U]):PLens[S,U]	=
			PLens { s =>
				for {
					thisStore	<- this on s
					thatStore	<- that on thisStore.get
				}
				yield {
					Store[S,U](
						thatStore.get,
						thatStore.put andThen thisStore.put
					)
				}
			}
			
	def xmapContainer[R](bijection:Bijection[R,S]):PLens[R,T]	=
			PLens {
				it => on(bijection write it) map { _ map bijection.read }
			}
		
	def xmapValue[U](bijection:Bijection[T,U]):PLens[S,U]	=
			PLens { s =>
				on(s) map { _ xmapValue bijection }
			}
			
	def over[R](store:Option[Store[R,S]]):Option[Store[R,T]]	=
			for {
				store	<- store
				here	<- this on store.get
			}
			yield store andThen here
			
	def overTotal[R](store:Store[R,S]):Option[Store[R,T]]	=
			this on store.get map { _ compose store }
			
	// impossible
	// def zip[U](that:PLens[S,U]):PLens[S,(T,U)]	=
	
	// ||| 
	def sum[SS](that:PLens[SS,T]):PLens[Either[S,SS],T]	=
			PLens { 
				_ match {
					case Left(s)	=>
						this on s map { store =>
							Store[Either[S,SS],T](
								store.get,
								it => Left(store put it)
							)
						}
					case Right(ss)	=>
						that on ss map { store =>
							Store[Either[S,SS],T](
								store.get,
								it => Right(store put it)
							)
						}
				}
			}
			
	// ***
	def product[SS,TT](that:PLens[SS,TT]):PLens[(S,SS),(T,TT)]	= 
			PLens { case (s,ss)	=>
				for {
					thisStore	<- this on s
					thatStore	<- that on ss
				}
				yield {
					Store[(S,SS),(T,TT)](
						(thisStore.get, thatStore.get),
						{ case (t,tt) => (thisStore put t, thatStore put tt) }
					)
				}
			}
}
