package scutil.lang

import scutil.core.implicits._
import scutil.lang.tc._

object EitherT {
	def pure[F[_]:Applicative,L,R](it:R):EitherT[F,L,R]		= right(it)
	def pureF[F[_]:Functor,L,R](it:F[R]):EitherT[F,L,R]		= rightF(it)

	def error[F[_]:Applicative,L,R](it:L):EitherT[F,L,R]	= left(it)
	def errorF[F[_]:Functor,L,R](it:F[L]):EitherT[F,L,R]	= leftF(it)

	//------------------------------------------------------------------------------

	def right[F[_]:Applicative,L,R](it:R):EitherT[F,L,R]	= fromEither(Either right it)
	def rightF[F[_]:Functor,L,R](it:F[R]):EitherT[F,L,R]	= EitherT(it map Right.apply)

	def left[F[_]:Applicative,L,R](it:L):EitherT[F,L,R]		= fromEither(Either left it)
	def leftF[F[_]:Functor,L,R](it:F[L]):EitherT[F,L,R]		= EitherT(it map Left.apply)

	//------------------------------------------------------------------------------

	def fromEither[F[_],L,R](it:Either[L,R])(implicit M:Applicative[F]):EitherT[F,L,R]	=
		EitherT(M pure it)

	def switch[F[_]:Applicative,L,R](condition:Boolean, falseLeft: =>L, trueRight: =>R):EitherT[F,L,R]	=
		fromEither(Either.cond(condition, trueRight, falseLeft))

	//------------------------------------------------------------------------------

	// inference helper
	def pureU[F[_],L]:EitherTPure[F,L]	= new EitherTPure[F,L]
	final class EitherTPure[F[_],L] {
		def apply[T](it:T)(implicit F:Applicative[F]):EitherT[F,L,T]	= EitherT pure it
	}

	// inference helper
	def errorU[F[_],R]:EitherTError[F,R]	= new EitherTError[F,R]
	final class EitherTError[F[_],R] {
		def apply[T](it:T)(implicit F:Applicative[F]):EitherT[F,T,R]	= EitherT error it
	}

	// inference helper
	def wrap[F[_]]:Wrap[F]	= new Wrap[F]
	final class Wrap[F[_]] {
		def apply[L,R](it:F[Either[L,R]]):EitherT[F,L,R]							= EitherT(it)
		def either[L,R](it:Either[L,R])(implicit M:Applicative[F]):EitherT[F,L,R]	= fromEither(it)
		// left and right need more type parameters
	}

	//------------------------------------------------------------------------------

	def delay[F[_],L,R](it: =>R)(implicit D:Delay[F]):EitherT[F,L,R]	=
		EitherT(D delay Right(it))

	def delayFromEither[F[_],L,R](it: =>Either[L,R])(implicit D:Delay[F]):EitherT[F,L,R]	=
		EitherT(D delay it)

	def delayCatching[F[_]:Delay,T](it: =>T):EitherT[F,Exception,T]	=
		delayFromEither(Catch.exception in it)

	//------------------------------------------------------------------------------

	implicit final class MergeableEitherT[F[_],T](peer:EitherT[F,T,T]) {
		def merge(implicit F:Functor[F]):F[T]	= (F map peer.value)(_.merge)
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def EitherTDelay[F[_]:Delay,L]:Delay[EitherT[F,L,*]]	=
		new Delay[EitherT[F,L,*]] {
			override def delay[R](it: =>R):EitherT[F,L,R]	= EitherT delay it
		}

	implicit def EitherTMonad[F[_]:Monad,L]:Monad[EitherT[F,L,*]]	=
		new Monad[EitherT[F,L,*]] {
			override def pure[R](it:R):EitherT[F,L,R]												= EitherT pure it
			override def map[R,RR](it:EitherT[F,L,R])(func:R=>RR):EitherT[F,L,RR]					= it map func
			override def flatMap[R,RR](it:EitherT[F,L,R])(func:R=>EitherT[F,L,RR]):EitherT[F,L,RR]	= it flatMap func
		}
}

final case class EitherT[F[_],L,R](value:F[Either[L,R]]) {
	@deprecated("use mapK", "0.196.0")
	def transform[G[_]](nat:F ~> G):EitherT[G,L,R]	=
		mapK(nat)

	def mapK[G[_]](nat:F ~> G):EitherT[G,L,R]	=
		transformFunc(nat.apply)

	def transformFunc[G[_]](func:F[Either[L,R]]=>G[Either[L,R]]):EitherT[G,L,R]	=
		EitherT(func(value))

	//------------------------------------------------------------------------------

	def map[RR](func:R=>RR)(implicit M:Functor[F]):EitherT[F,L,RR]	=
		transform(_ map func)

	def flatMap[RR](func:R=>EitherT[F,L,RR])(implicit M:Monad[F]):EitherT[F,L,RR]	=
		EitherT(
			(M flatMap value) {
				_.map(func).cata (
					Either.left[L,RR] _ andThen M.pure,
					_.value
				)
			}
		)

	def flatten[RR](implicit ev:R=>EitherT[F,L,RR], M:Monad[F]):EitherT[F,L,RR]	=
		flatMap(ev)

	//------------------------------------------------------------------------------

	def leftMap[LL](func:L=>LL)(implicit M:Functor[F]):EitherT[F,LL,R]	=
		transform(_ leftMap func)

	def leftFlatMap[LL](func:L=>EitherT[F,LL,R])(implicit M:Monad[F]):EitherT[F,LL,R]	=
		EitherT(
			(M flatMap value) {
				_.leftMap(func).cata (
					_.value,
					Either.right[LL,R] _ andThen M.pure
				)
			}
		)

	def leftFlatten[LL](implicit ev:L=>EitherT[F,LL,R], M:Monad[F]):EitherT[F,LL,R]	=
		leftFlatMap(ev)

	//------------------------------------------------------------------------------

	def bimap[LL,RR](leftFunc:L=>LL, rightFunc:R=>RR)(implicit M:Functor[F]):EitherT[F,LL,RR]	=
		transform(_.bimap(leftFunc, rightFunc))


	@deprecated("use transform", "0.196.0")
	def mapEither[LL,RR](func:Either[L,R]=>Either[LL,RR])(implicit M:Functor[F]):EitherT[F,LL,RR]	=
		transform(func)

	def transform[LL,RR](func:Either[L,R]=>Either[LL,RR])(implicit M:Functor[F]):EitherT[F,LL,RR]	=
		EitherT((M map value)(func))

	@deprecated("use subflatMap", "0.196.0")
	def flatMapEither[LL,RR](func:R=>Either[L,RR])(implicit M:Functor[F]):EitherT[F,L,RR]	=
		subflatMap(func)

	def subflatMap[LL,RR](func:R=>Either[L,RR])(implicit M:Functor[F]):EitherT[F,L,RR]	=
		transform(_ flatMap func)

	//------------------------------------------------------------------------------

	def orElse[LL>:L,RR>:R](that: =>EitherT[F,LL,RR])(implicit M:Monad[F]):EitherT[F,LL,RR]	=
		EitherT(
			(M flatMap value) {
				case Left(_)	=> that.value
				case x			=> M pure x
			}
		)

	@deprecated("use orElseT", "0.196.0")
	def orElsePure[LL>:L,RR>:R](that: =>Either[LL,RR])(implicit M:Functor[F]):EitherT[F,LL,RR]	=
		orElseT(that)

	def orElseT[LL>:L,RR>:R](that: =>Either[LL,RR])(implicit M:Functor[F]):EitherT[F,LL,RR]	=
		EitherT(
			(M map value) {
				case Left(_)	=> that
				case x			=> x
			}
		)

	//------------------------------------------------------------------------------

	@deprecated("use toOption", "0.196.0")
	def toOptionT(implicit M:Functor[F]):OptionT[F,R]	=
		toOption

	def toOption(implicit M:Functor[F]):OptionT[F,R]	=
		OptionT((M map value)(_.toOption))

	//------------------------------------------------------------------------------

	def swap(implicit M:Functor[F]):EitherT[F,R,L]	=
		EitherT(M.map(value)(_.swap))

	def getOrElse(default: =>R)(implicit M:Functor[F]):F[R]	=
		M.map(value)(_ getOrElse default)
}
