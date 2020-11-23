package scutil.lang

import scutil.core.implicits._
import scutil.lang.tc._

object WriterT {
	def pure[F[_],L:Monoid,T](it:T)(implicit M:Applicative[F]):WriterT[F,L,T]	=
		liftF(M pure it)

	// TODO Delay should inherit from Monad (or Applicative?) so we don't need a Functor here
	def delay[F[_]:Delay,L:Monoid,T](it: =>T)(implicit D:Delay[F], F:Functor[F]):WriterT[F,L,T]	=
		liftF(D delay it)

	def liftF[F[_],L:Monoid,T](io:F[T])(implicit F:Functor[F]):WriterT[F,L,T]	=
		WriterT {
			io map { v =>
				Monoid[L].empty -> v
			}
		}

	def liftL[F[_],L](log:L)(implicit F:Applicative[F]):WriterT[F,L,Unit]	=
		WriterT(
			F pure (
				log -> (())
			)
		)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def WriterTDelay[F[_]:Delay:Functor,L:Monoid]:Delay[WriterT[F,L,*]]	=
		new Delay[WriterT[F,L,*]] {
			def delay[T](it: =>T):WriterT[F,L,T]	= WriterT delay it
		}

	implicit def WriterTMonad[F[_]:Monad,L:Monoid]:Monad[WriterT[F,L,*]]	=
		new Monad[WriterT[F,L,*]] {
			def pure[T](it:T):WriterT[F,L,T]											= WriterT pure it
			def flatMap[S,T](its:WriterT[F,L,S])(func:S=>WriterT[F,L,T]):WriterT[F,L,T]	= its flatMap func
		}
}

final case class WriterT[F[_],L,T](run:F[(L,T)]) {
	def map[U](func:T=>U)(implicit F:Functor[F]):WriterT[F,L,U]	=
		WriterT {
			(F map run) { case (s,t) =>
				(s, func(t))
			}
		}

	def flatMap[U](func:T=>WriterT[F,L,U])(implicit M:Monad[F], SG:Semigroup[L]):WriterT[F,L,U]	=
		WriterT {
			for {
				tmp1	<-	this.run
				(e1, s)	=	tmp1
				tmp2	<-	func(s).run
				(e2, t)	=	tmp2
			}
			yield {
				val	fin	= SG.concat(e1, e2)
				fin -> t
			}
		}
}
