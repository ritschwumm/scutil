package scutil.lang

import scutil.lang.implicits._
import scutil.lang.tc._

object PLens extends PLensInstances {
	def identity[T]:PLens[T,T]	=
			PLens { t =>
				Some(Store identity t)
			}
		
	def trivial[T]:PLens[T,Unit]	=
			PLens { t =>
				Some(Store trivial t)
			}
		
	def always[T]:PLens[Option[T],T]	=
			Prism.always[T].toPLens
		
	def codiag[T]:PLens[Either[T,T],T]	=
			identity[T] sum identity[T]
}

// TODO optics rework
final case class PLens[S,T](on:S=>Option[Store[S,T]]) {
	def get(s:S):Option[T]		= on(s) map { _.get }
	def getter:PFunction[S,T]	= get(_)
	
	def put(s:S, t:T):Option[S]	= on(s) map { _ set t }
	def putter(t:T):PEndo[S]	= put(_, t)
	
	def modify(s:S, func:Endo[T]):Option[S]	= on(s) map { _ modify func }
	def modifier(func:Endo[T]):PEndo[S]		= modify(_, func)
	
	def modifyF[F[_]:Functor](s:S, func:FEndo[F,T]):Option[F[S]]	= on(s) map { _ modifyF func }
	def modifierF[F[_]:Functor](func:FEndo[F,T]):S=>Option[F[S]]	= modifyF(_, func)
	
	def modifyState[X](s:S, func:State[T,X]):Option[(S,X)]	= on(s) map { _ modifyState func }
	def modifierState[X](func:State[T,X]):S=>Option[(S,X)]	= modifyState(_, func)
	
	def modifyStateT[F[_]:Functor,X](s:S, func:StateT[F,T,X]):Option[F[(S,X)]]	= on(s) map (_ modifyStateT func)
	def modifierStateT[F[_]:Functor,X](func:StateT[F,T,X]):S=>Option[F[(S,X)]]	= modifyStateT(_, func)
	
	//------------------------------------------------------------------------------

	// BETTER use these to replace modifier
	def modifyP(s:S):Option[Endo[T]=>S]							= on(s) map { _.modify }
	def modifyPF[F[_]:Functor](s:S):Option[FEndo[F,T]=>F[S]]	= on(s) map { _.modifyF }
	
	def modifyStateTPF[F[_]:Functor,X](s:S):Option[StateT[F,T,X]=>F[(S,X)]]	= on(s) map { _.modifyStateT }
	
	//------------------------------------------------------------------------------
	
	def orElse(that:PLens[S,T]):PLens[S,T]	=
			PLens(this.on orElse that.on)
		
	/** symbolic alias for andThen */
	def >=>[U](that:PLens[T,U]):PLens[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:PLens[R,S]):PLens[R,T]	=
			this compose that
		
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
						thatStore.set andThen thisStore.set
					)
				}
			}
			
	//------------------------------------------------------------------------------
			
	def over[R](store:Option[Store[R,S]]):Option[Store[R,T]]	=
			for {
				store	<- store
				here	<- this on store.get
			}
			yield store andThen here
			
	def overTotal[R](store:Store[R,S]):Option[Store[R,T]]	=
			this on store.get map { _ compose store }
			
	//------------------------------------------------------------------------------
	
	// impossible
	// def zip[U](that:PLens[S,U]):PLens[S,(T,U)]
	
	// |||
	def sum[SS](that:PLens[SS,T]):PLens[Either[S,SS],T]	=
			PLens {
				_ match {
					case Left(s)	=>
						this on s map { store =>
							Store[Either[S,SS],T](
								store.get,
								it => Left(store set it)
							)
						}
					case Right(ss)	=>
						that on ss map { store =>
							Store[Either[S,SS],T](
								store.get,
								it => Right(store set it)
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
						{ case (t,tt) => (thisStore set t, thatStore set tt) }
					)
				}
			}
			
	//------------------------------------------------------------------------------
	
	// TODO optics does this make sense?
	def toLens(default: =>Store[S,T]):Lens[S,T]	=
			Lens fromStoreAt { on(_) getOrElse default }
}

trait PLensInstances {
	implicit def PLensSemigroup[S,T]:Semigroup[PLens[S,T]]	=
			Semigroup instance (_ orElse _)
}