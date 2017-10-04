package scutil.lang.pimp

import scutil.lang._
import scutil.lang.tc._

object BooleanImplicits extends BooleanImplicits

trait BooleanImplicits {
	implicit final class BooleanExt(peer:Boolean) {
		def cata[T](falseValue: =>T, trueValue: =>T):T =
				if (peer)	trueValue
				else		falseValue
				
		//------------------------------------------------------------------------------
		
		def option[T](trueValue: =>T):Option[T] =
				if (peer)	Some(trueValue)
				else		None
		
		def optionNot[T](falseValue: =>T):Option[T] =
				if (!peer)	Some(falseValue)
				else		None
			
		def list[T](trueValue: =>T):List[T]	=
				if (peer)	List(trueValue)
				else		List.empty
				
		def listNot[T](falseValue: =>T):List[T]	=
				if (!peer)	List(falseValue)
				else		List.empty
				
		def vector[T](trueValue: =>T):Vector[T]	=
				if (peer)	Vector(trueValue)
				else		Vector.empty
				
		def vectorNot[T](falseValue: =>T):Vector[T]	=
				if (!peer)	Vector(falseValue)
				else		Vector.empty
				
		def set[T](trueValue: =>T):Set[T]	=
				if (peer)	Set(trueValue)
				else		Set.empty
				
		def setNot[T](falseValue: =>T):Set[T]	=
				if (!peer)	Set(falseValue)
				else		Set.empty
				
		def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] =
				if (peer)	Right(trueRight)
				else		Left(falseLeft)
			
		def validated[E,T](falseProblems: =>E, trueGood: =>T):Validated[E,T]	=
				Validated switch (peer, falseProblems, trueGood)
			
		//------------------------------------------------------------------------------
		
		def flatOption[T](trueValue: =>Option[T]):Option[T] =
				if (peer)	trueValue
				else		None
		
		def flatOptionNot[T](falseValue: =>Option[T]):Option[T] =
				if (!peer)	falseValue
				else		None
			
		//------------------------------------------------------------------------------
			
		def optionT[F[_]:Applicative,T](trueValue: =>T):OptionT[F,T] =
				OptionT fromOption option(trueValue)
		
		def optionNotT[F[_]:Applicative,T](falseValue: =>T):OptionT[F,T] =
				OptionT fromOption optionNot(falseValue)
			
		//------------------------------------------------------------------------------
		
		// TODO generalize to MonadPlus (?)
		
		def guardOption:Option[Unit]	=
				option(())
			
		def preventOption:Option[Unit]	=
				optionNot(())
			
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
				guardVector(trueValue)
		
		def preventISeq[T](falseValue: =>T):ISeq[T] =
				preventVector(falseValue)
			
		def guardVector[T](trueValue: =>T):Vector[T] =
				if (peer)	Vector(trueValue)
				else		Vector.empty
		
		def preventVector[T](falseValue: =>T):ISeq[T] =
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
		
		def trueEffect(effect: =>Unit):Boolean	= { if (peer)	effect; peer }
		def falseEffect(effect: =>Unit):Boolean	= { if (!peer)	effect; peer }
	}
}
