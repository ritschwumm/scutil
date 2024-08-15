package scutil.lang

import scutil.lang.tc.*

object StateT { outer =>
	def pure[F[_],S,T](it:T)(using F:Applicative[F]):StateT[F,S,T]	=
		StateT { s => F.pure(s -> it) }

	def pureF[F[_],S,T](it:F[T])(using F:Functor[F]):StateT[F,S,T]	=
		StateT { s => F.map(it) { s -> _ } }

	def fromState[F[_],S,T](it:State[S,T])(using F:Applicative[F]):StateT[F,S,T]	=
		fromStateFunc(it.run)

	def fromStateFunc[F[_],S,T](it:S=>(S,T))(using F:Applicative[F]):StateT[F,S,T]	=
		StateT { s => F.pure(it(s)) }

	def transformPureF[M[_],S](using F:Functor[M]):M ~> StateT[M,S,_]	=
		new (M ~> StateT[M,S,_]) {
			def apply[X](it:M[X]):StateT[M,S,X]	=
				StateT.pureF(it)
		}

	//------------------------------------------------------------------------------

	def delay[F[_],S,T](it: =>T)(using D:Delay[F]):StateT[F,S,T]	=
		StateT { s => D.delay(s -> it) }

	//------------------------------------------------------------------------------

	def get[F[_],S](using F:Applicative[F]):StateT[F,S,S]	=
		StateT { s => F.pure(s -> s) }

	//------------------------------------------------------------------------------

	def set[F[_],S](it:S)(using F:Applicative[F]):StateT[F,S,Unit]	=
		StateT { s => F.pure(it -> (())) }

	def setF[F[_],S](it:F[S])(using F:Functor[F]):StateT[F,S,Unit]	=
		StateT { s => F.map(it)(s1 => (s1, ())) }

	def setOld[F[_],S](it:S)(using F:Applicative[F]):StateT[F,S,S]	=
		StateT { s => F.pure(it -> s) }

	//------------------------------------------------------------------------------

	def mod[F[_],S](func:S=>S)(using F:Applicative[F]):StateT[F,S,Unit]	=
		StateT { s => F.pure(func(s) -> (())) }

	def modF[F[_],S](func:S=>F[S])(using F:Functor[F]):StateT[F,S,Unit]	=
		StateT { s => F.map(func(s))(s1 => (s1, ())) }

	def modOld[F[_],S](func:S=>S)(using F:Applicative[F]):StateT[F,S,S]	=
		StateT { s => F.pure(func(s) -> s) }

	//------------------------------------------------------------------------------

	// == get map func
	def stateless[F[_],S,T](func:S=>T)(using F:Applicative[F]):StateT[F,S,T]	=
		StateT { s => F.pure(s -> func(s)) }

	def statelessF[F[_],S,T](func:S=>F[T])(using F:Functor[F]):StateT[F,S,T]	=
		StateT { s => F.map(func(s)) { s -> _ } }

	//------------------------------------------------------------------------------
	//## typeclass instances

	given StateTDelay[F[_]:Delay,S]:Delay[StateT[F,S,_]]	=
		new Delay[StateT[F,S,_]] {
			override def delay[T](it: =>T):StateT[F,S,T]	= StateT.delay(it)
		}

	given StateTMonad[F[_]:Monad,S]:Monad[StateT[F,S,_]]	=
		new Monad[StateT[F,S,_]] {
			override def pure[T](it:T):StateT[F,S,T]											= StateT.pure(it)
			override def map[T,U](its:StateT[F,S,T])(func:T=>U):StateT[F,S,U]					= its.map(func)
			override def flatMap[T,U](its:StateT[F,S,T])(func:T=>StateT[F,S,U]):StateT[F,S,U]	= its.flatMap(func)
		}
}

final case class StateT[F[_],S,T](run:S=>F[(S,T)]) {
	def mapK[G[_]](nat:F ~> G):StateT[G,S,T]	=
		StateT { s0 =>
			nat(run(s0))
		}

	def transformFunc[G[_]](func:(S=>F[(S,T)])=>(S=>G[(S,T)])):StateT[G,S,T]	=
		StateT { s0 =>
			func(run)(s0)
		}

	//------------------------------------------------------------------------------

	def map[U](func:T=>U)(using F:Functor[F]):StateT[F,S,U]	=
		StateT { s0 =>
			F.map(run(s0)) { (s1, t) => (s1, func(t)) }
		}

	def flatMap[U](func:T=>StateT[F,S,U])(using F:Monad[F]):StateT[F,S,U]	=
		StateT { s0 =>
			F.flatMap(run(s0)) { (s1, t) =>
				func(t).run(s1)
			}
		}

	def flatten[U](using F:Monad[F], ev:T <:< StateT[F,S,U]):StateT[F,S,U]	=
		flatMap(ev)

	/** function effect first */
	def ap[A,B](that:StateT[F,S,A])(using F:Monad[F], ev:T <:< (A=>B)):StateT[F,S,B]	=
		that.pa(this.map(ev))

	/** function effect first */
	def pa[U](that:StateT[F,S,T=>U])(using F:Monad[F]):StateT[F,S,U]	=
		StateT { s0 =>
			F.flatMap(that.run(s0)) { (s1, tu) =>
				F.map(this.run(s1)) { (s2, t) =>
					s2 -> tu(t)
				}
			}
		}

	def product[U](that:StateT[F,S,U])(using F:Monad[F]):StateT[F,S,(T,U)]	=
		this.map2(that)(_ -> _)

	def map2[U,X](that:StateT[F,S,U])(func:(T,U)=>X)(using F:Monad[F]):StateT[F,S,X]	=
		StateT { s0 =>
			F.flatMap(this.run(s0)) { (s1, t) =>
				F.map(that.run(s1)) { (s2, u) =>
					s2 -> func(t, u)
				}
			}
		}

	def subMap[U](func:F[T]=>F[U])(using F:Monad[F]):StateT[F,S,U]	=
		StateT { s0 =>
			F.flatMap(run(s0)) { (s1, t) =>
				F.map(func(F.pure(t))) { u =>
					(s1 -> u)
				}
			}
		}

	def subFlatMap[U](func:T=>F[U])(using F:Monad[F]):StateT[F,S,U]	=
		StateT { s0 =>
			F.flatMap(run(s0)) { (s1, t) =>
				F.map(func(t)) { u =>
					(s1 -> u)
				}
			}
		}

	def subFlatten[U](using F:Monad[F], ev:T <:< F[U]):StateT[F,S,U]	=
		subFlatMap(ev)

	def innerFlatMap[U](func:F[T]=>StateT[F,S,U])(using F:Monad[F]):StateT[F,S,U]	=
		StateT { s0 =>
			F.flatMap(run(s0)) { (s1, t) =>
				func(F.pure(t)).run(s1)
			}
		}

	def innerFlatten[U](using F:Monad[F], ev:F[T] <:< StateT[F,S,U]):StateT[F,S,U]	=
		innerFlatMap(ev)
}
