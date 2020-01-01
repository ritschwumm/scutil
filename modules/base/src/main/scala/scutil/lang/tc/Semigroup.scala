package scutil.lang.tc

object Semigroup {
	def apply[F](implicit ev:Semigroup[F]):Semigroup[F]	= ev

	def instance[T](func:(T,T)=>T):Semigroup[T]	=
		new Semigroup[T] {
			def concat(a:T, b:T):T	= func(a, b)
		}
}

trait Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	def concat(a:F, b:F):F
}
