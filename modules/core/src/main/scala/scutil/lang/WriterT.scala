package scutil.lang

import scutil.core.implicits.*
import scutil.lang.tc.*

object WriterT {
	def pure[F[_],L:Monoid,T](it:T)(using M:Applicative[F]):WriterT[F,L,T]	=
		liftF(M.pure(it))

	// TODO Delay should inherit from Monad (or Applicative?) so we don't need a Functor here
	def delay[F[_]:Delay,L:Monoid,T](it: =>T)(using D:Delay[F], F:Functor[F]):WriterT[F,L,T]	=
		liftF(D.delay(it))

	def liftF[F[_],L:Monoid,T](io:F[T])(using F:Functor[F]):WriterT[F,L,T]	=
		WriterT {
			io.map { v =>
				Monoid[L].empty -> v
			}
		}

	def liftL[F[_],L](log:L)(using F:Applicative[F]):WriterT[F,L,Unit]	=
		WriterT(
			F.pure(
				log -> (())
			)
		)

	//------------------------------------------------------------------------------
	//## typeclass instances

	given WriterTDelay[F[_]:Delay:Functor,L:Monoid]:Delay[WriterT[F,L,_]]	=
		new Delay[WriterT[F,L,_]] {
			def delay[T](it: =>T):WriterT[F,L,T]	= WriterT.delay(it)
		}

	given WriterTMonad[F[_]:Monad,L:Monoid]:Monad[WriterT[F,L,_]]	=
		new Monad[WriterT[F,L,_]] {
			def pure[T](it:T):WriterT[F,L,T]											= WriterT.pure(it)
			def flatMap[S,T](its:WriterT[F,L,S])(func:S=>WriterT[F,L,T]):WriterT[F,L,T]	= its.flatMap(func)
		}
}

final case class WriterT[F[_],L,T](run:F[(L,T)]) {
	def map[U](func:T=>U)(using F:Functor[F]):WriterT[F,L,U]	=
		WriterT {
			F.map(run) { (s,t) =>
				(s, func(t))
			}
		}

	def flatMap[U](func:T=>WriterT[F,L,U])(using M:Monad[F], SG:Semigroup[L]):WriterT[F,L,U]	=
		WriterT {
			for {
				tmp1	<-	this.run
				(e1, s)	=	tmp1
				tmp2	<-	func(s).run
				(e2, t)	=	tmp2
			}
			yield {
				val	fin	= SG.combine(e1, e2)
				fin -> t
			}
		}
}
