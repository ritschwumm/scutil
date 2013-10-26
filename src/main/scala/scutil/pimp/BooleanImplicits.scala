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
	
	def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] =
			if (peer)	Right(trueRight)
			else		Left(falseLeft)
		
	def tried[U,V](falseFail: =>U, trueWin: =>V):Tried[U,V] =
			if (peer)	Win(trueWin)
			else		Fail(falseFail)
		
	//------------------------------------------------------------------------------
	
	// BETTER rename these to guard and prevent?
	
	def trueSome:Option[Unit]	=
			if (peer)	Some(())
			else		None
	
	def falseSome:Option[Unit]	=
			if (!peer)	Some(())
			else		None
		
	def trueWin[U](problem: =>U):Tried[U,Unit]	=
			if (peer)	Win(())
			else		Fail(problem)
	
	def falseWin[U](problem: =>U):Tried[U,Unit]	=
			if (!peer)	Win(())
			else		Fail(problem)
		
	//------------------------------------------------------------------------------
		
	def trueEffect(effect: =>Unit):Boolean	= { if (peer)	effect; peer }
	def falseEffect(effect: =>Unit):Boolean	= { if (!peer)	effect; peer }
}
