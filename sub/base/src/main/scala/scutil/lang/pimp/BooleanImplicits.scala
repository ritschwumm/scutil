package scutil.lang.pimp

import scutil.lang._
import scutil.lang.tc._

object BooleanImplicits extends BooleanImplicits

trait BooleanImplicits {
	implicit final class BooleanExt(peer:Boolean) {
		def cata[T](falseValue: =>T, trueValue: =>T):T =
				if (peer)	trueValue
				else		falseValue
				
		def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] =
				if (peer)	Right(trueRight)
				else		Left(falseLeft)
			
		def validated[E,T](falseProblems: =>E, trueGood: =>T):Validated[E,T]	=
				Validated switch (peer, falseProblems, trueGood)
			
		//------------------------------------------------------------------------------
		
		// TODO rename these to when and unless
		
		def guard[T](trueValue: =>T):Option[T] =
				if (peer)	Some(trueValue)
				else		None
		
		def prevent[T](falseValue: =>T):Option[T] =
				if (!peer)	Some(falseValue)
				else		None
			
		def flatGuard[T](trueValue: =>Option[T]):Option[T] =
				if (peer)	trueValue
				else		None
		
		def flatPrevent[T](falseValue: =>Option[T]):Option[T] =
				if (!peer)	falseValue
				else		None
				
		def guardT[F[_]:Applicative,T](trueValue: =>T):OptionT[F,T] =
				OptionT fromOption guard(trueValue)
		
		def preventT[F[_]:Applicative,T](falseValue: =>T):OptionT[F,T] =
				OptionT fromOption prevent(falseValue)
			
		//------------------------------------------------------------------------------
		
		def guardOption:Option[Unit]	=
				guard(())
			
		def preventOption:Option[Unit]	=
				prevent(())
			
		def guardEither[U](leftValue: =>U):Either[U,Unit]	=
				if (peer)	Right(())
				else		Left(leftValue)
		
		def preventEither[U](leftValue: =>U):Either[U,Unit]	=
				if (!peer)	Right(())
				else		Left(leftValue)
			
		def guardValidated[E](problems: =>E):Validated[E,Unit]	=
				if (peer)	Good(())
				else		Bad(problems)
			
		def preventValidated[E](problems: =>E):Validated[E,Unit]	=
				if (!peer)	Good(())
				else		Bad(problems)
			
		def guardISeq[T](trueValue: =>T):ISeq[T] =
				if (peer)	Vector(trueValue)
				else		Vector.empty
		
		def preventISeq[T](falseValue: =>T):ISeq[T] =
				if (!peer)	Vector(falseValue)
				else		Vector.empty
			
		//------------------------------------------------------------------------------
		
		def guardOptionT[F[_]:Applicative]:OptionT[F,Unit]	=
				OptionT fromOption guardOption
			
		def preventOptionT[F[_]:Applicative]:OptionT[F,Unit]	=
				OptionT fromOption preventOption
			
		def guardEitherT[F[_]:Applicative,U](leftValue: =>U):EitherT[F,U,Unit]	=
				EitherT switch (peer, leftValue, ())
			
		def preventEitherT[F[_]:Applicative,U](leftValue: =>U):EitherT[F,U,Unit]	=
				EitherT switch (!peer, leftValue, ())
			
		//------------------------------------------------------------------------------
		
		@deprecated("use guardOption", "0.119.0")
		def trueSome:Option[Unit]	=
				guardOption
		
		@deprecated("use preventOption", "0.119.0")
		def falseSome:Option[Unit]	=
				preventOption
			
		@deprecated("use guardEither", "0.119.0")
		def trueRight[U](problem: =>U):Either[U,Unit]	=
				guardEither(problem)
		
		@deprecated("use preventEither", "0.119.0")
		def falseRight[U](problem: =>U):Either[U,Unit]	=
				preventEither(problem)
			
		@deprecated("use guardValidated", "0.119.0")
		def trueValidated[E](problems: =>E):Validated[E,Unit]	=
				Validated goodCondition (peer, problems)
			
		@deprecated("use preventValidated", "0.119.0")
		def falseValidated[E](problems: =>E):Validated[E,Unit]	=
				Validated badCondition (peer, problems)
			
		//------------------------------------------------------------------------------
		
		def trueEffect(effect: =>Unit):Boolean	= { if (peer)	effect; peer }
		def falseEffect(effect: =>Unit):Boolean	= { if (!peer)	effect; peer }
	}
}
