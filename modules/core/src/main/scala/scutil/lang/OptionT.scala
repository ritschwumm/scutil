package scutil.lang

import scutil.lang.tc._

object OptionT {
	def pure[F[_]:Applicative,T](it:T):OptionT[F,T]					= some(it)
	def pureF[F[_],T](it:F[T])(implicit M:Functor[F]):OptionT[F,T]	= someF(it)

	//------------------------------------------------------------------------------

	def some[F[_]:Applicative,T](it:T):OptionT[F,T]					= fromOption(Some(it))
	def someF[F[_],T](it:F[T])(implicit M:Functor[F]):OptionT[F,T]	= OptionT((M map it)(Some.apply))

	def none[F[_]:Applicative,T]:OptionT[F,T]						= fromOption(None)

	//------------------------------------------------------------------------------

	def fromOption[F[_],T](it:Option[T])(implicit M:Applicative[F]):OptionT[F,T]	=
		OptionT(M pure it)

	def switch[F[_]:Applicative,T](condition:Boolean, trueSome: =>T):OptionT[F,T]	=
		fromOption(if (condition) Some(trueSome) else None)

	//------------------------------------------------------------------------------

	// inference helper
	def pureU[F[_]]:OptionTPure[F]	= new OptionTPure[F]
	final class OptionTPure[F[_]] {
		def apply[T](it:T)(implicit F:Applicative[F]):OptionT[F,T]	= OptionT pure it
	}

	// inference helper
	def wrap[F[_]]:Wrap[F]	= new Wrap[F]
	final class Wrap[F[_]] {
		def apply[T](it:F[Option[T]]):OptionT[F,T]							= OptionT(it)
		def option[T](it:Option[T])(implicit M:Applicative[F]):OptionT[F,T]	= fromOption(it)
		def some[T](it:T)(implicit M:Monad[F]):OptionT[F,T]					= OptionT some it
		def none[T](implicit M:Monad[F]):OptionT[F,T]						= OptionT.none
		def delay[T](it: =>T)(implicit D:Delay[F]):OptionT[F,T]				= OptionT delay it
	}

	//------------------------------------------------------------------------------

	def delay[F[_],T](it: =>T)(implicit D:Delay[F]):OptionT[F,T]	= OptionT(D delay Some(it))

	def delayFromOption[F[_],T](it: =>Option[T])(implicit D:Delay[F]):OptionT[F,T]	=
		OptionT(D delay it)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def OptionTDelay[F[_]:Delay]:Delay[OptionT[F,*]]	=
		new Delay[OptionT[F,*]] {
			override def delay[T](it: =>T):OptionT[F,T]	= OptionT delay it
		}

	implicit def OptionTMonad[F[_]](implicit MF:Monad[F]):Monad[OptionT[F,*]]	=
		new Monad[OptionT[F,*]] {
			override def pure[T](it:T):OptionT[F,T]											= OptionT pure it
			override def map[S,T](its:OptionT[F,S])(func:S=>T):OptionT[F,T]					= its map func
			override def flatMap[S,T](its:OptionT[F,S])(func:S=>OptionT[F,T]):OptionT[F,T]	= its flatMap func
		}
}

final case class OptionT[F[_],T](value:F[Option[T]]) {
	@deprecated("use mapK", "0.196.0")
	def transform[G[_]:Monad](nat:F ~> G):OptionT[G,T]	=
		mapK(nat)

	def mapK[G[_]:Monad](nat:F ~> G):OptionT[G,T]	=
		transformFunc(nat.apply)

	def transformFunc[G[_]:Monad](func:F[Option[T]]=>G[Option[T]]):OptionT[G,T]	=
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

	@deprecated("use orElseT", "0.196.0")
	def orElsePure[TT>:T](that:Option[TT])(implicit M:Functor[F]):OptionT[F,TT]	=
		orElseT(that)

	def orElseT[TT>:T](that:Option[TT])(implicit M:Functor[F]):OptionT[F,TT]	=
		OptionT(
			(M map value) {
				case None	=> that
				case x		=> x
			}
		)

	def getOrElseF[TT>:T](that:F[TT])(implicit M:Monad[F]):F[TT]	=
		(M flatMap value) {
			case None		=> that
			case Some(x)	=> M pure x
		}

	@deprecated("use getOrElse", "0.196.0")
	def getOrElsePure[TT>:T](that:TT)(implicit M:Functor[F]):F[TT]	=
		getOrElse(that)

	def getOrElse[TT>:T](that:TT)(implicit M:Functor[F]):F[TT]	=
		(M map value) {
			case None		=> that
			case Some(x)	=> x
		}

	//------------------------------------------------------------------------------

	def toRightF[L](leftValue: =>F[L])(implicit M:Monad[F]):EitherT[F,L,T]	=
		EitherT {
			(M flatMap value) {
				case Some(x)	=> M pure Right(x)
				case None		=> (M map leftValue)(Left.apply)
			}
		}

	@deprecated("use toRight", "0.196.0")
	def toRightPure[L](leftValue: =>L)(implicit M:Functor[F]):EitherT[F,L,T]	=
		toRight(leftValue)

	def toRight[L](leftValue: =>L)(implicit M:Functor[F]):EitherT[F,L,T]	=
		EitherT((M map value)(_ toRight leftValue))

	def toLeftF[R](rightValue: =>F[R])(implicit M:Monad[F]):EitherT[F,T,R]	=
		EitherT {
			(M flatMap value) {
				case Some(x)	=> M pure Left(x)
				case None		=> (M map rightValue)(Right.apply)
			}
		}

	@deprecated("use toLeft", "0.196.0")
	def toLeftPure[R](rightValue: =>R)(implicit M:Functor[F]):EitherT[F,T,R]	=
		toLeft(rightValue)

	def toLeft[R](rightValue: =>R)(implicit M:Functor[F]):EitherT[F,T,R]	=
		EitherT((M map value)(_ toLeft rightValue))
}
