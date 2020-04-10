package scutil.lang.tc

object Semigroup {
	def apply[F](implicit ev:Semigroup[F]):Semigroup[F]	= ev

	def instance[T](func:(T,T)=>T):Semigroup[T]	=
		new Semigroup[T] {
			def concat(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def concat[T](a:T, b:T)(implicit SG:Semigroup[T]):T	=
		SG.concat(a, b)

	// TODO concatAll for Nes without the need for an empty element
}

trait Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	def concat(a:F, b:F):F
}
