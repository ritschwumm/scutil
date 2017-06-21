package scutil.lang.pimp

import scutil.lang._

// TODO either get rid of this
import scutil.lang.pimp.EitherImplicits._

object BooleanImplicits extends BooleanImplicits

trait BooleanImplicits {
	implicit final class BooleanExt(peer:Boolean) {
		def cata[T](falseValue: =>T, trueValue: =>T):T =
				if (peer)	trueValue
				else		falseValue
		
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
				
		// BETTER generalize this
		
		def guardISeq[T](trueValue: =>T):ISeq[T] =
				if (peer)	Vector(trueValue)
				else		Vector.empty
		
		def preventISeq[T](falseValue: =>T):ISeq[T] =
				if (!peer)	Vector(falseValue)
				else		Vector.empty
				
		//------------------------------------------------------------------------------
		
		def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] =
				if (peer)	Right(trueRight)
				else		Left(falseLeft)
			
		def validated[E,T](falseProblems: =>E, trueGood: =>T):Validated[E,T]	=
				Validated switch (peer, falseProblems, trueGood)
			
		//------------------------------------------------------------------------------
		
		// BETTER rename these to guard and prevent?
		
		def trueSome:Option[Unit]	=
				if (peer)	Some(())
				else		None
		
		def falseSome:Option[Unit]	=
				if (!peer)	Some(())
				else		None
			
		def trueRight[U](problem: =>U):Either[U,Unit]	=
				Either rightCondition (peer, problem)
		
		def falseRight[U](problem: =>U):Either[U,Unit]	=
				Either leftCondition (peer, problem)
			
		def trueValidated[E](problems: =>E):Validated[E,Unit]	=
				Validated goodCondition (peer, problems)
			
		def falseValidated[E](problems: =>E):Validated[E,Unit]	=
				Validated badCondition (peer, problems)
			
		//------------------------------------------------------------------------------
			
		def trueEffect(effect: =>Unit):Boolean	= { if (peer)	effect; peer }
		def falseEffect(effect: =>Unit):Boolean	= { if (!peer)	effect; peer }
	}
}
