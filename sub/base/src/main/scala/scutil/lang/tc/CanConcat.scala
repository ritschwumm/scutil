package scutil.lang.tc

object CanConcat extends CanConcatInstances {
	def apply[F:CanConcat]:CanConcat[F]	= implicitly[CanConcat[F]]
	
	def by[T](func:(T,T)=>T):CanConcat[T]	=
			new CanConcat[T] {
				def concat(a:T, b:T):T	= func(a, b)
			}
}

/** aka Semigroup */
trait CanConcat[F] {
	def concat(a:F, b:F):F
}
