package scutil.lang

import scutil.lang.tc._

object StateT extends StateTInstances {
	def delay[F[_],S,T](it: =>T)(implicit F:Applicative[F]):StateT[F,S,T]	=
			StateT { s => F pure (s -> it)	}
	def delayThunk[F[_],S,T](it:()=>T)(implicit F:Applicative[F]):StateT[F,S,T]	=
			StateT { s => F pure (s -> it()) }
	
	def pure[F[_],S,T](it:T)(implicit F:Applicative[F]):StateT[F,S,T]	=
			StateT { s => F pure (s -> it) }
		
	def pureF[F[_],S,T](it:F[T])(implicit F:Functor[F]):StateT[F,S,T]	=
			StateT { s => (F map it) { s -> _ } }
		
	// get map func
	def stateless[F[_],S,T](func:S=>T)(implicit F:Applicative[F]):StateT[F,S,T]	=
			StateT { s => F pure (s -> func(s)) }
		
	def statelessF[F[_],S,T](func:S=>F[T])(implicit F:Functor[F]):StateT[F,S,T]	=
			StateT { s => (F map func(s)) { s -> _ } }
			
	def get[F[_],S,T](implicit F:Applicative[F]):StateT[F,S,S]	=
			StateT { s => F pure (s -> s) }
		
	def set[F[_],S,T](it:S)(implicit F:Applicative[F]):StateT[F,S,Unit]	=
			StateT { s => F pure (it -> (())) }
		
	def setOld[F[_],S,T](it:S)(implicit F:Applicative[F]):StateT[F,S,S]	=
			StateT { s => F pure (it -> s) }
		
	def mod[F[_],S,T](func:S=>S)(implicit F:Applicative[F]):StateT[F,S,Unit]	=
			StateT { s => F pure (func(s) -> (())) }
		
	def modOld[F[_],S,T](func:S=>S)(implicit F:Applicative[F]):StateT[F,S,S]	=
			StateT { s => F pure (func(s) -> s) }
		
	// inference helper allowing to specify the state value typ while still let the result type be inferred
	def pureU[F[_],S]:StateTPure[F,S]	= new StateTPure[F,S]
	final class StateTPure[F[_],S] {
		def apply[T](it:T)(implicit F:Applicative[F]):StateT[F,S,T]	= StateT pure it
	}
}

final case class StateT[F[_],S,T](run:S=>F[(S,T)]) {
	def transform[G[_]](func:F ~> G):StateT[G,S,T]	=
			StateT { s0 =>
				func(run(s0))
			}
	
	def inside[R](lens:TLens[R,S])(implicit F:Functor[F]):StateT[F,R,T]	= 
			StateT { r0	=>
				lens on r0 modifyStateT this
			}
		
	def map[U](func:T=>U)(implicit F:Functor[F]):StateT[F,S,U]	=
			StateT { s0 =>
				(F map run(s0)) { case (s1, t) => (s1, func(t)) }
			}
			
	def flatMap[U](func:T=>StateT[F,S,U])(implicit F:Monad[F]):StateT[F,S,U]	=
			StateT { s0 =>
				(F flatMap run(s0)) { case (s1, t) =>
					func(t) run s1
				}
			}
			
	def flatten[U](implicit ev:T=>StateT[F,S,U], F:Monad[F]):StateT[F,S,U]	=
			flatMap(ev)
			
	/** function effect first */
	def ap[A,B](that:StateT[F,S,A])(implicit F:Monad[F], ev:T=>(A=>B)):StateT[F,S,B]	=
			that pa (this map ev)
			
	/** function effect first */
	def pa[U](that:StateT[F,S,T=>U])(implicit F:Monad[F]):StateT[F,S,U]	=
			StateT { s0 =>
				(F flatMap (that run s0)) { case (s1, tu) =>
					(F map (this run s1)) { case (s2, t) =>
						s2 -> tu(t)
					}
				}
			}
			
	def zip[U](that:StateT[F,S,U])(implicit F:Monad[F]):StateT[F,S,(T,U)]	=
			(this zipWith that)(_ -> _)
			
	def zipWith[U,X](that:StateT[F,S,U])(func:(T,U)=>X)(implicit F:Monad[F]):StateT[F,S,X]	=
			StateT { s0 =>
				(F flatMap (this run s0)) { case (s1, t) =>
					(F map (that run s1)) { case (s2, u) =>
						s2 -> func(t, u)
					}
				}
			}
			
	def subMap[U](func:F[T]=>F[U])(implicit F:Monad[F]):StateT[F,S,U]	=
			StateT { s0 =>
				(F flatMap run(s0)) { case (s1, t) =>
					(F map func(F pure t)) { u =>
						(s1 -> u)
					}
				}
			}
			
	def subFlatMap[U](func:T=>F[U])(implicit F:Monad[F]):StateT[F,S,U]	=
			StateT { s0 =>
				(F flatMap run(s0)) { case (s1, t) =>
					(F map func(t)) { u =>
						(s1 -> u)
					}
				}
			}
			
	def subFlatten[U](implicit ev:T=>F[U], F:Monad[F]):StateT[F,S,U]	=
			subFlatMap(ev)
	
	def innerFlatMap[U](func:F[T]=>StateT[F,S,U])(implicit F:Monad[F]):StateT[F,S,U]	=
			StateT { s0 =>
				(F flatMap run(s0)) { case (s1, t) =>
					func(F pure t) run s1
				}
			}
	
	def innerFlatten[U](implicit ev:F[T]=>StateT[F,S,U], F:Monad[F]):StateT[F,S,U]	=
			innerFlatMap(ev)
}

trait StateTInstances {
	implicit def StateTMonad[F[_]:Monad,S]:Monad[StateT[F,S,?]]	=
			new Monad[StateT[F,S,?]] {
				override def pure[T](it:T):StateT[F,S,T]											= StateT pure it
				override def map[T,U](its:StateT[F,S,T])(func:T=>U):StateT[F,S,U]					= its map func
				override def flatMap[T,U](its:StateT[F,S,T])(func:T=>StateT[F,S,U]):StateT[F,S,U]	= its flatMap func
			}
			
	implicit def StateTDelay[F[_]:Applicative,S]:Delay[StateT[F,S,?]]	=
			new Delay[StateT[F,S,?]] {
				override def delay[T](it: =>T):StateT[F,S,T]			= StateT delay it
				override def delayThunk[T](it:Thunk[T]):StateT[F,S,T]	= StateT delayThunk it
			}
}
