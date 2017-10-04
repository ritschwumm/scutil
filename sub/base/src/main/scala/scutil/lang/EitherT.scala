package scutil.lang

import scala.util.Try

import scutil.base.implicits._
import scutil.lang.tc._

object EitherT extends EitherTInstances {
	def pure[F[_]:Applicative,L,R](it:R):EitherT[F,L,R]	= right(it)
	
	def right[F[_]:Applicative,L,R](it:R):EitherT[F,L,R]	= fromEither(Either right it)
	def left[F[_]:Applicative,L,R](it:L):EitherT[F,L,R]	= fromEither(Either left it)
	
	def fromEither[F[_],L,R](it:Either[L,R])(implicit M:Applicative[F]):EitherT[F,L,R]	=
			EitherT(M pure it)
		
	def pureF[F[_]:Functor,L,R](it:F[R]):EitherT[F,L,R]		= rightF(it)
	def rightF[F[_]:Functor,L,R](it:F[R]):EitherT[F,L,R]	= EitherT(it map Right.apply)
	def leftF[F[_]:Functor,L,R](it:F[L]):EitherT[F,L,R]		= EitherT(it map Left.apply)
 
	def wrap[F[_]]:Wrap[F]	= new Wrap[F]
	final class Wrap[F[_]] { 
		def apply[L,R](it:F[Either[L,R]]):EitherT[F,L,R]							= EitherT(it)
		def either[L,R](it:Either[L,R])(implicit M:Applicative[F]):EitherT[F,L,R]	= fromEither(it)
		// left and right need more type parameters
	}
	
	//------------------------------------------------------------------------------
		
	/*
	@deprecated("use Validated#toEitherT", "0.119.0")
	def fromValidated[F[_],L,R](it:Validated[L,R])(implicit M:Applicative[F]):EitherT[F,L,R]	=
			EitherT(M pure it.toEither)
	*/
	
	@deprecated("use Try#toEitherT", "0.119.0")
	def fromTry[F[_],R](it:Try[R])(implicit M:Applicative[F]):EitherT[F,Throwable,R]	=
			it.toEitherT
	
	//------------------------------------------------------------------------------
	
	def delay[F[_]:Delay,L,R](it: =>R):EitherT[F,L,R]		= delayWin(it)
	
	def delayWin[F[_]:Delay,L,R](it: =>R):EitherT[F,L,R]	= delayFromEither(Either right it)
	def delayFail[F[_]:Delay,L,R](it: =>L):EitherT[F,L,R]	= delayFromEither(Either left it)
		
	def delayFromEither[F[_],L,R](it: =>Either[L,R])(implicit D:Delay[F]):EitherT[F,L,R]	=
			EitherT(D delay it)
		
	def delayCatching[F[_]:Delay,T](it: =>T):EitherT[F,Exception,T]	=
			delayFromEither(Catch.exception in it)
		
	//------------------------------------------------------------------------------
	
	def thunk[F[_]:Delay,L,R](it:Thunk[R]):EitherT[F,L,R]		= thunkWin(it)
	
	def thunkWin[F[_]:Delay,L,R](it:Thunk[R]):EitherT[F,L,R]	= thunkFromEither(() => Either right it())
	def thunkFail[F[_]:Delay,L,R](it:Thunk[L]):EitherT[F,L,R]	= thunkFromEither(() => Either left it())
		
	def thunkFromEither[F[_],L,R](it:Thunk[Either[L,R]])(implicit D:Delay[F]):EitherT[F,L,R]	=
			EitherT(D thunk it)
		
	def thunkCatching[F[_]:Delay,T](it:Thunk[T]):EitherT[F,Exception,T]	=
			thunkFromEither(() => Catch.exception in it())
		
	//------------------------------------------------------------------------------
	
	def switch[F[_]:Applicative,L,R](condition:Boolean, falseLeft: =>L, trueRight: =>R):EitherT[F,L,R]	=
			fromEither(Either cond (condition, trueRight, falseLeft))
		
	//------------------------------------------------------------------------------
		
	@deprecated("use Boolean#guardEitherT", "0.119.0")
	def rightCondition[F[_]:Applicative,L](flag:Boolean, leftValue: =>L):EitherT[F,L,Unit]	=
			flag guardEitherT leftValue
		
	@deprecated("use Option#toRightPureT", "0.119.0")
	def fromOption[F[_]:Monad,L,R](right:Option[R], leftValue: =>L):EitherT[F,L,R]	=
			right toRightPureT leftValue
		
	@deprecated("use OptionT(x)#toRightPure", "0.119.0")
	def fromOptionF[F[_]:Functor,L,R](right:F[Option[R]], leftValue: =>L):EitherT[F,L,R]	=
			OptionT(right) toRightPure leftValue
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
				override def delay[R](it: =>R):EitherT[F,L,R]		= EitherT delay it
				override def thunk[R](it:Thunk[R]):EitherT[F,L,R]	= EitherT thunk it
			}
			
	implicit def EitherTMonad[F[_]:Monad,L]:Monad[EitherT[F,L,?]]	=
			new Monad[EitherT[F,L,?]] {
				override def pure[R](it:R):EitherT[F,L,R]												= EitherT pure it
				override def map[R,RR](it:EitherT[F,L,R])(func:R=>RR):EitherT[F,L,RR]					= it map func
				override def flatMap[R,RR](it:EitherT[F,L,R])(func:R=>EitherT[F,L,RR]):EitherT[F,L,RR]	= it flatMap func
			}
}
