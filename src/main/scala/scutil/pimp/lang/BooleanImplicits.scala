package scutil.pimp

import scutil.lang._

object BooleanImplicits extends BooleanImplicits

trait BooleanImplicits {
	implicit def toBooleanExt(peer:Boolean) = new BooleanExt(peer)
}

final class BooleanExt(peer:Boolean) {
	def cata[T](falseValue: =>T, trueValue: =>T):T =
			if (peer)	trueValue
			else		falseValue
	
	def guard[T](trueSome: =>T):Option[T] =
			if (peer)	Some(trueSome)
			else		None
	
	def prevent[T](falseSome: =>T):Option[T] =
			if (!peer)	Some(falseSome)
			else		None
	
	def flatGuard[T](trueValue: =>Option[T]):Option[T] = 
			if (peer)	trueValue
			else		None
	
	def flatPrevent[T](falseValue: =>Option[T]):Option[T] =
			if (!peer)	falseValue
			else		None
			
	//------------------------------------------------------------------------------
	
	def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] =
			if (peer)	Right(trueRight)
			else		Left(falseLeft)
		
	def tried[U,V](falseFail: =>U, trueWin: =>V):Tried[U,V] =
			Tried switch (peer, falseFail, trueWin)
		
	def validated[E,T](falseProblems: =>Nes[E], trueGood: =>T):Validated[E,T]	=
			Validated switch (peer, falseProblems, trueGood)
		
	//------------------------------------------------------------------------------
	
	// BETTER rename these to guard and prevent?
	
	def trueSome:Option[Unit]	=
			if (peer)	Some(())
			else		None
	
	def falseSome:Option[Unit]	=
			if (!peer)	Some(())
			else		None
		
	def trueWin[U](problem: =>U):Tried[U,Unit]	=
			Tried winCondition (peer, problem)
	
	def falseWin[U](problem: =>U):Tried[U,Unit]	=
			Tried failCondition (peer, problem)
		
	def trueValidated[E](problems: =>Nes[E]):Validated[E,Unit]	=
			Validated goodCondition (peer, problems)
		
	def falseValidated[E](problems: =>Nes[E]):Validated[E,Unit]	=
			Validated badCondition (peer, problems)
		
	//------------------------------------------------------------------------------
		
	def trueEffect(effect: =>Unit):Boolean	= { if (peer)	effect; peer }
	def falseEffect(effect: =>Unit):Boolean	= { if (!peer)	effect; peer }
}
