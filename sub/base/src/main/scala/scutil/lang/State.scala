package scutil.lang

import scutil.lang.tc._

object State extends StateInstances {
	def delay[S,T](it: =>T):State[S,T]			= State { s => (s,	it)	}
	def delayThunk[S,T](it:Thunk[T]):State[S,T]	= State { s => (s,	it())	}
	
	def pure[S,T](it:T):State[S,T]		= State { s => (s,			it)	}
	def get[S]:State[S,S]				= State { s => (s,			s)	}
	def set[S](it:S):State[S,Unit]		= State { s => (it,			())	}
	def setOld[S](it:S):State[S,S]		= State { s => (it,			s)	}
	def mod[S](func:S=>S):State[S,Unit]	= State { s => (func(s),	())	}
	def modOld[S](func:S=>S):State[S,S]	= State { s => (func(s),	s)	}
	
	// inference helper allowing to specifiy the state value typ while still let the result type be inferred
	def pureU[S]:StatePure[S]	= new StatePure[S]
	final class StatePure[S] {
		def apply[T](it:T):State[S,T]	= State pure it
	}
	
	//------------------------------------------------------------------------------
	
	def optionalT[S,T](func:StateT[Option,S,T]):State[S,Option[T]]	=
			optional(func.run)
		
	def optional[S,T](run:S=>Option[(S,T)]):State[S,Option[T]]	=
			State { s1 =>
				run(s1) match {
					case None			=> (s1, None)
					case Some((s2, t))	=> (s2, Some(t))
				}
			}
			
	def optionalMod[S](func:Option[S=>S]):State[S,Unit]	=
			State mod { s =>
				func map (_ apply s) getOrElse s
			}
		
	def modOptional[S](func:S=>Option[S]):State[S,Unit]	=
			State mod { s =>
				func(s) getOrElse s
			}
}

final case class State[S,+T](run:S=>(S,T)) {
	def inside[R](lens:TLens[R,S]):State[R,T]	=
			State { r1	=>
				lens on r1 modifyState this
			}
	
	def map[U](func:T=>U):State[S,U]	=
			State { s1 =>
				val (s2, t)	= run(s1)
				(s2, func(t))
			}
			
	def flatMap[U](func:T=>State[S,U]):State[S,U]	=
			State { s1 =>
				val (s2, t)	= run(s1)
				func(t).run(s2)
			}
			
	/** function effect first */
	def ap[A,B](that:State[S,A])(implicit ev:T=>(A=>B)):State[S,B]	=
			that pa (this map ev)
			
	/** function effect first */
	def pa[U](that:State[S,T=>U]):State[S,U]	=
			State { s =>
				val (s1, tu)	= that run s
				val (s2, t)		= this run s1
				(s2, tu(t))
			}
			
	def zip[U](that:State[S,U]):State[S,(T,U)]	=
			(this zipWith that)(_ -> _)
			
	// aka combine
	def zipWith[U,X](that:State[S,U])(func:(T,U)=>X):State[S,X]	=
			State { s =>
				val (s1, t)	= this run s
				val (s2, u)	= that run s1
				(s2, func(t, u))
			}
}

trait StateInstances {
	implicit def StateMonad[S]:Monad[State[S,?]]	=
			new Monad[State[S,?]] {
				override def pure[T](it:T):State[S,T]										= State pure it
				override def map[T,U](its:State[S,T])(func:T=>U):State[S,U]					= its map func
				override def flatMap[T,U](its:State[S,T])(func:T=>State[S,U]):State[S,U]	= its flatMap func
			}
			
	implicit def StateDelay[S]:Delay[State[S,?]]	=
			new Delay[State[S,?]] {
				override def delay[T](it: =>T):State[S,T]		= State delay it
				override def delayThunk[T](it:()=>T):State[S,T]	= State delayThunk it
			}
}
