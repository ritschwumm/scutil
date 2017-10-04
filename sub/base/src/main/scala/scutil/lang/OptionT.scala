package scutil.lang

import scutil.lang.tc._

object OptionT extends OptionTInstances {
	def pure[F[_]:Applicative,T](it:T):OptionT[F,T]	= some(it)
	
	def some[F[_]:Applicative,T](it:T):OptionT[F,T]	= fromOption(Some(it))
	def none[F[_]:Applicative,T]:OptionT[F,T]		= fromOption(None)
	
	def fromOption[F[_],T](it:Option[T])(implicit M:Applicative[F]):OptionT[F,T]	=
			OptionT(M pure it)
	
	def pureF[F[_],T](it:F[T])(implicit M:Functor[F]):OptionT[F,T]	= pureF(it)
	def someF[F[_],T](it:F[T])(implicit M:Functor[F]):OptionT[F,T]	= OptionT((M map it)(Some.apply))
	
	def wrap[F[_]]:Wrap[F]	= new Wrap[F]
	final class Wrap[F[_]] { 
		def apply[T](it:F[Option[T]]):OptionT[F,T]							= OptionT(it)
		def option[T](it:Option[T])(implicit M:Applicative[F]):OptionT[F,T]	= fromOption(it)
		def some[T](it:T)(implicit M:Monad[F]):OptionT[F,T]					= OptionT some it
		def none[T](implicit M:Monad[F]):OptionT[F,T]						= OptionT.none
	}
	
	//------------------------------------------------------------------------------
	
	def delay[F[_]:Delay,T](it: =>T):OptionT[F,T]		= delaySome(it)
	def delaySome[F[_]:Delay,T](it: =>T):OptionT[F,T]	= delayFromOption(Some(it))
		
	def delayFromOption[F[_],T](it: =>Option[T])(implicit D:Delay[F]):OptionT[F,T]	=
			OptionT(D delay it)
		
	//------------------------------------------------------------------------------
	
	def thunk[F[_]:Delay,T](it:Thunk[T]):OptionT[F,T]		= thunkSome(it)
	def thunkSome[F[_]:Delay,T](it:Thunk[T]):OptionT[F,T]	= thunkFromOption(() => Some(it()))
		
	def thunkFromOption[F[_],T](it:Thunk[Option[T]])(implicit D:Delay[F]):OptionT[F,T]	=
			OptionT(D thunk it)
}

final case class OptionT[F[_],T](value:F[Option[T]]) {
	def reval[G[_]:Monad](func:F[Option[T]]=>G[Option[T]]):OptionT[G,T]	=
			OptionT(func(value))
		
	//------------------------------------------------------------------------------
		
	def map[U](func:T=>U)(implicit M:Functor[F]):OptionT[F,U]	=
			OptionT(
				M.map(value)(
					_ map func
				)
			)
		
	def flatMap[U](func:T=>OptionT[F,U])(implicit M:Monad[F]):OptionT[F,U]	=
			OptionT(
				M.flatMap(value)(
					_ map (func(_).value) getOrElse (M pure None)
				)
			)
			
	def flatten[U](implicit ev:T=>OptionT[F,U], M:Monad[F]):OptionT[F,U]	=
			flatMap(ev)
		
	//------------------------------------------------------------------------------
	
	def mapOption[U](func:Option[T]=>Option[U])(implicit M:Functor[F]):OptionT[F,U]	=
			OptionT(
				M.map(value)(func)
			)
		
	def flatMapOption[U](func:T=>Option[U])(implicit M:Monad[F]):OptionT[F,U]	=
			OptionT(
				M.map(value)(
					_ flatMap func
				)
			)
			
	def flattenOption[U](implicit ev:T=>Option[U], M:Monad[F]):OptionT[F,U]	=
			flatMapOption(ev)
			
	//------------------------------------------------------------------------------
			
	def orElse[TT>:T](that:OptionT[F,TT])(implicit M:Monad[F]):OptionT[F,TT]	=
			OptionT(
				(M flatMap value) {
					case None	=> that.value
					case x		=> M pure x
				}
			)
			
	def orElsePure[TT>:T](that:Option[TT])(implicit M:Functor[F]):OptionT[F,TT]	=
			OptionT(
				(M map value) {
					case None	=> that
					case x		=> x
				}
			)
	
	def getOrElse[TT>:T](that:F[TT])(implicit M:Monad[F]):F[TT]	=
			(M flatMap value) {
				case None		=> that
				case Some(x)	=> M pure x
			}
	
	def getOrElsePure[TT>:T](that:TT)(implicit M:Functor[F]):F[TT]	=
			(M map value) {
				case None		=> that
				case Some(x)	=> x
			}
			
	//------------------------------------------------------------------------------
	
	def toRight[L](leftValue: =>F[L])(implicit M:Monad[F]):EitherT[F,L,T]	=
			EitherT {
				(M flatMap value) {
					case Some(x)	=> M pure Right(x)
					case None		=> (M map leftValue)(Left.apply)
				}
			}
			
	def toRightPure[L](leftValue: =>L)(implicit M:Functor[F]):EitherT[F,L,T]	=
			EitherT((M map value)(_ toRight leftValue))
		
	def toLeft[R](rightValue: =>F[R])(implicit M:Monad[F]):EitherT[F,T,R]	=
			EitherT {
				(M flatMap value) {
					case Some(x)	=> M pure Left(x)
					case None		=> (M map rightValue)(Right.apply)
				}
			}
			
	def toLeftPure[R](rightValue: =>R)(implicit M:Functor[F]):EitherT[F,T,R]	=
			EitherT((M map value)(_ toLeft rightValue))
}

trait OptionTInstances {
	implicit def OptionTDelay[F[_]:Delay]:Delay[OptionT[F,?]]	=
			new Delay[OptionT[F,?]] {
				override def delay[T](it: =>T):OptionT[F,T]		= OptionT delay it
				override def thunk[T](it:Thunk[T]):OptionT[F,T]	= OptionT thunk it
			}
			
	implicit def OptionTMonad[F[_]](implicit MF:Monad[F]):Monad[OptionT[F,?]]	=
			new Monad[OptionT[F,?]] {
				override def pure[T](it:T):OptionT[F,T]											= OptionT pure it
				override def map[S,T](its:OptionT[F,S])(func:S=>T):OptionT[F,T]					= its map func
				override def flatMap[S,T](its:OptionT[F,S])(func:S=>OptionT[F,T]):OptionT[F,T]	= its flatMap func
			}
}
