package scutil.lang.tc

object Monoid {
	def apply[F](implicit ev:Monoid[F]):Monoid[F]	= ev

	def instance[T](empty1:T, func:(T,T)=>T):Monoid[T]	=
		new Monoid[T] {
			def empty:T				= empty1
			def concat(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def empty[T](implicit SG:Monoid[T]):T	= SG.empty

	def concatOf[T:Monoid](xs:T*):T	=
		concatAll(xs)

	// TODO add a Foldable typeclass
	def concatAll[T](xs:Iterable[T])(implicit SG:Monoid[T]):T	=
		 (xs foldLeft SG.empty)(SG.concat)
}

trait Monoid[F] extends Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	def empty:F
}
