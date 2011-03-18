package scutil.ext

object BooleanImplicits extends BooleanImplicits

trait BooleanImplicits {
	implicit def toBooleanExt(delegate:Boolean) = new BooleanExt(delegate)
}

final class BooleanExt(delegate:Boolean) {
	def fold[T](trueValue: =>T, falseValue: =>T):T = delegate match {
		case true	=> trueValue
		case false	=> falseValue
	}
	
	def guard[T](trueSome: =>T):Option[T] = delegate match {
		case true	=> Some(trueSome)
		case false	=> None
	}
	
	def prevent[T](falseSome: =>T):Option[T] = delegate match {
		case true	=> None
		case false	=> Some(falseSome)
	}
	
	def either[U,V](falseLeft: =>U, trueRight: =>V):Either[U,V] = delegate match {
		case true	=> Right(trueRight)
		case false	=> Left(falseLeft)
	}
	
	def trueEffect(effect: =>Unit):Boolean	= { if ( delegate) effect; delegate }
	def falseEffect(effect: =>Unit):Boolean	= { if (!delegate) effect; delegate }
}
