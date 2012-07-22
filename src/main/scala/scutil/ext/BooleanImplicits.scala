package scutil.ext

import scutil.data._

object BooleanImplicits extends BooleanImplicits

trait BooleanImplicits {
	implicit def toBooleanExt(delegate:Boolean) = new BooleanExt(delegate)
}

final class BooleanExt(delegate:Boolean) {
	def cata[T](trueValue: =>T, falseValue: =>T):T =
			if (delegate)	trueValue
			else			falseValue
	
	def guard[T](trueSome: =>T):Option[T] =
			if (delegate)	Some(trueSome)
			else			None
	
	def prevent[T](falseSome: =>T):Option[T] =
			if (!delegate)	Some(falseSome)
			else			None
	
	def flatGuard[T](trueValue: =>Option[T]):Option[T] = 
			if (delegate)	trueValue
			else			None
	
	def flatPrevent[T](falseValue: =>Option[T]):Option[T] =
			if (!delegate)	falseValue
			else			None
	
	def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] =
			if (delegate)	Right(trueRight)
			else			Left(falseLeft)
		
	def tried[U,V](falseFail: =>U, trueWin: =>V):Tried[U,V] =
			if (delegate)	Win(trueWin)
			else			Fail(falseFail)
	
	def trueEffect(effect: =>Unit):Boolean	= { if (delegate)	effect; delegate }
	def falseEffect(effect: =>Unit):Boolean	= { if (!delegate)	effect; delegate }
}
