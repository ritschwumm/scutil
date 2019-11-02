package scutil.lang

object Bijections {
	def withDefault[T](default:T):Bijection[Option[T],T]	=
			Bijection(
				get	= state => state getOrElse default,
				set	= value => if (value != default)	Some(value) else None
			)
}
