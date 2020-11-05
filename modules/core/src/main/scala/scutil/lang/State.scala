package scutil.lang

import scutil.lang.tc._

object State {
	def pure[S,T](it:T):State[S,T]		= State { s => (s,			it)	}
	def get[S]:State[S,S]				= State { s => (s,			s)	}
	def set[S](it:S):State[S,Unit]		= State { s => (it,			())	}
	def setOld[S](it:S):State[S,S]		= State { s => (it,			s)	}
	def mod[S](func:S=>S):State[S,Unit]	= State { s => (func(s),	())	}
	def modOld[S](func:S=>S):State[S,S]	= State { s => (func(s),	s)	}

	//------------------------------------------------------------------------------

	// inference helper allowing to specifiy the state value typ while still let the result type be inferred
	def pureU[S]:StatePure[S]	= new StatePure[S]
	final class StatePure[S] {
		def apply[T](it:T):State[S,T]	= State pure it
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def StateMonad[S]:Monad[State[S,?]]	=
		new Monad[State[S,?]] {
			override def pure[T](it:T):State[S,T]										= State pure it
			override def map[T,U](its:State[S,T])(func:T=>U):State[S,U]					= its map func
			override def flatMap[T,U](its:State[S,T])(func:T=>State[S,U]):State[S,U]	= its flatMap func
		}
}

final case class State[S,+T](run:S=>(S,T)) {
	def runState(it:S):S	= run(it)._1
	def runResult(it:S):T	= run(it)._2

	//------------------------------------------------------------------------------

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

	def tuple[U](that:State[S,U]):State[S,(T,U)]	=
		(this map2 that)(_ -> _)

	def map2[U,X](that:State[S,U])(func:(T,U)=>X):State[S,X]	=
		State { s =>
			val (s1, t)	= this run s
			val (s2, u)	= that run s1
			(s2, func(t, u))
		}

	//------------------------------------------------------------------------------

	def toStateT[F[_],TT>:T](implicit M:Applicative[F]):StateT[F,S,TT]	=
		StateT fromState this
}
