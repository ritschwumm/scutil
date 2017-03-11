package scutil.lang

import scutil.lang.tc._

object State extends StateInstances {
	def pure[S,T](it:T):State[S,T]		= State { s => (s,			it)	}
	def get[S]:State[S,S]				= State { s => (s,			s)	}
	def set[S](it:S):State[S,Unit]		= State { s => (it,			())	}
	def setOld[S](it:S):State[S,S]		= State { s => (it,			s)	}
	def mod[S](func:S=>S):State[S,Unit]	= State { s => (func(s),	())	}
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
			
	def ap[A,B](that:State[S,A])(implicit ev:T=>(A=>B)):State[S,B]	=
			State { s =>
				val (s1, ab)	= this run s
				val (s2, a)	= that run s1
				(s2, ab(a))
			}
			
	def pa[U](that:State[S,T=>U]):State[S,U]	=
			State { s =>
				val (s1, t)	= this run s
				val (s2, u)	= that run s1
				(s2, u(t))
			}
			
	def zip[U](that:State[S,U]):State[S,(T,U)]	=
			State { s =>
				val (s1, t)	= this run s
				val (s2, u)	= that run s1
				(s2, (t, u))
			}
			
	def zipWith[U,X](that:State[S,U])(func:(T,U)=>X):State[S,X]	=
			State { s =>
				val (s1, t)	= this run s
				val (s2, u)	= that run s1
				(s2, func(t, u))
			}
}

trait StateInstances {
	// ({type l[X]=State[S,X]})#l
	// State[S,?]
	implicit def StateMonad[S]:Monad[ ({type l[X]=State[S,X]})#l ]	=
			new Monad[ ({type l[X]=State[S,X]})#l ] {
				override def pure[T](it:T):State[S,T]										= State pure it
				override def map[T,U](its:State[S,T])(func:T=>U):State[S,U]					= its map func
				override def flatMap[T,U](its:State[S,T])(func:T=>State[S,U]):State[S,U]	= its flatMap func
			}
}
