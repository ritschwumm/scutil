package scutil.lang

import scutil.lang.tc._

object State extends StateInstances {
	def pure[S,T](it:T):State[S,T]		= State { s => (s,			it)	}
	def get[S]:State[S,S]				= State { s => (s,			s)	}
	def set[S](it:S):State[S,Unit]		= State { s => (it,			())	}
	def mod[S](func:S=>S):State[S,Unit]	= State { s => (func(s),	())	}
}

final case class State[S,+T](run:S=>(S,T)) {
	def embed[R](lens:TLens[R,S]):State[R,T]	=
			State { r1	=>
				lens on r1 modifyStateful run
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
