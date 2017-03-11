package scutil.lang.tc

object Monoid {
	def apply[F](implicit ev:Monoid[F]):Monoid[F]	= ev
	
	def by[T](empty1:T, func:(T,T)=>T):Monoid[T]	=
			new Monoid[T] {
				def empty:T				= empty1
				def concat(a:T, b:T):T	= func(a, b)
			}
}

trait Monoid[F] extends Semigroup[F] {
	def empty:F
}
