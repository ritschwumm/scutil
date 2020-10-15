package scutil.lang.tc

import scutil.lang.Nes

object Semigroup {
	def apply[F](implicit ev:Semigroup[F]):Semigroup[F]	= ev

	def instance[T](func:(T,T)=>T):Semigroup[T]	=
		new Semigroup[T] {
			def concat(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def concat[T](a:T, b:T)(implicit SG:Semigroup[T]):T	=
		SG.concat(a, b)

	def concatOf1[T:Semigroup](x:T, xs:T*)	=
		concatAll1(Nes(x, xs))

	// TODO add a Foldable1 typeclass
	def concatAll1[T](all:Nes[T])(implicit SG:Semigroup[T]):T	=
		all.reduce(SG.concat)
}

trait Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	// TODO in cats this is named combine
	def concat(a:F, b:F):F
}
