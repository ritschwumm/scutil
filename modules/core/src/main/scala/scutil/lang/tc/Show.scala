package scutil.lang.tc

object Show {
	def apply[T](implicit ev:Show[T]):Show[T]	= ev

	def instance[T](func:T=>String):Show[T]	=
		new Show[T] {
			def show(it:T):String	= func(it)
		}

	def toStringInstance[T]:Show[T]	=
		Show instance (_.toString)

	def doit[T](value:T)(implicit S:Show[T]):String	= S show value
}

trait Show[-T] {
	def show(it:T):String

	def contraMap[S](func:S=>T):Show[S]	=
		Show instance { func andThen show }
}
