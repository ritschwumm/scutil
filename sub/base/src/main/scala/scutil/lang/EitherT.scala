package scutil.lang

import scutil.base.implicits._
import scutil.lang.tc._

object EitherT extends EitherTInstances {
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
			fromEither(Either cond (condition, trueRight, falseLeft))

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
		
	def delayPure[F[_]:Delay,L,R](it: =>R):EitherT[F,L,R]	= delayRight(it)
	def delayError[F[_]:Delay,L,R](it: =>L):EitherT[F,L,R]	= delayLeft(it)
		
	def delayRight[F[_]:Delay,L,R](it: =>R):EitherT[F,L,R]	= delayFromEither(Either right it)
	def delayLeft[F[_]:Delay,L,R](it: =>L):EitherT[F,L,R]	= delayFromEither(Either left it)
		
	def delayFromEither[F[_],L,R](it: =>Either[L,R])(implicit D:Delay[F]):EitherT[F,L,R]	=
			EitherT(D delay it)
		
	def delayCatching[F[_]:Delay,T](it: =>T):EitherT[F,Exception,T]	=
			delayFromEither(Catch.exception in it)
}

final case class EitherT[F[_],L,R](value:F[Either[L,R]]) {
	def reval[G[_]](func:F[Either[L,R]]=>G[Either[L,R]]):EitherT[G,L,R]	=
			EitherT(func(value))
	
	//------------------------------------------------------------------------------
	
	def map[RR](func:R=>RR)(implicit M:Functor[F]):EitherT[F,L,RR]	=
			mapEither(_ map func)
		
	def flatMap[RR](func:R=>EitherT[F,L,RR])(implicit M:Monad[F]):EitherT[F,L,RR]	=
			EitherT(
				(M flatMap value) {
					_ map func cata (
						Either.left[L,RR] _ andThen M.pure,
						_.value
					)
				}
			)
			
	def flatten[RR](implicit ev:R=>EitherT[F,L,RR], M:Monad[F]):EitherT[F,L,RR]	=
			flatMap(ev)
			
	//------------------------------------------------------------------------------
	
	def mapLeft[LL](func:L=>LL)(implicit M:Functor[F]):EitherT[F,LL,R]	=
			mapEither(_ mapLeft func)
		
	def flatMapLeft[LL](func:L=>EitherT[F,LL,R])(implicit M:Monad[F]):EitherT[F,LL,R]	=
			EitherT(
				(M flatMap value) {
					_ mapLeft func cata (
						_.value,
						Either.right[LL,R] _ andThen M.pure
					)
				}
			)
			
	def flattenLeft[LL](implicit ev:L=>EitherT[F,LL,R], M:Monad[F]):EitherT[F,LL,R]	=
			flatMapLeft(ev)
		
	//------------------------------------------------------------------------------
	
	def bimap[LL,RR](leftFunc:L=>LL, rightFunc:R=>RR)(implicit M:Functor[F]):EitherT[F,LL,RR]	=
			mapEither(_ bimap (leftFunc, rightFunc))
				
	def mapEither[LL,RR](func:Either[L,R]=>Either[LL,RR])(implicit M:Functor[F]):EitherT[F,LL,RR]	=
			EitherT((M map value)(func))
		
	def flatMapEither[LL,RR](func:R=>Either[L,RR])(implicit M:Functor[F]):EitherT[F,L,RR]	=
			mapEither(_ flatMap func)
		
	//------------------------------------------------------------------------------
	
	def orElse[LL>:L,RR>:R](that: =>EitherT[F,LL,RR])(implicit M:Monad[F]):EitherT[F,LL,RR]	=
			EitherT(
				(M flatMap value) {
					case Left(_)	=> that.value
					case x			=> M pure x
				}
			)
			
	def orElsePure[LL>:L,RR>:R](that: =>Either[LL,RR])(implicit M:Functor[F]):EitherT[F,LL,RR]	=
			EitherT(
				(M map value) {
					case Left(_)	=> that
					case x			=> x
				}
			)
			
	//------------------------------------------------------------------------------
	
	def toOptionT(implicit M:Functor[F]):OptionT[F,R]	=
			OptionT((M map value)(_.toOption))
	
	//------------------------------------------------------------------------------
		
	// TODO merge, swap, getOrElse
}

trait EitherTInstances {
	implicit def EitherTDelay[F[_]:Delay,L]:Delay[EitherT[F,L,?]]	=
			new Delay[EitherT[F,L,?]] {
				override def delay[R](it: =>R):EitherT[F,L,R]	= EitherT delayPure it
			}
			
	implicit def EitherTMonad[F[_]:Monad,L]:Monad[EitherT[F,L,?]]	=
			new Monad[EitherT[F,L,?]] {
				override def pure[R](it:R):EitherT[F,L,R]												= EitherT pure it
				override def map[R,RR](it:EitherT[F,L,R])(func:R=>RR):EitherT[F,L,RR]					= it map func
				override def flatMap[R,RR](it:EitherT[F,L,R])(func:R=>EitherT[F,L,RR]):EitherT[F,L,RR]	= it flatMap func
			}
}
