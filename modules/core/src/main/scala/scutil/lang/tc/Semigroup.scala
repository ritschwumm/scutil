package scutil.lang.tc

import scutil.lang.Nes

object Semigroup {
	def apply[F](implicit ev:Semigroup[F]):Semigroup[F]	= ev

	def instance[T](func:(T,T)=>T):Semigroup[T]	=
		new Semigroup[T] {
			def concat(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def concat[T](a:T, b:T)(implicit T:Semigroup[T]):T	=
		T.concat(a, b)

	def concatOf1[T:Semigroup](x:T, xs:T*)	=
		concatAll1(Nes(x, xs))

	def concatAll1[T](items:Nes[T])(implicit T:Semigroup[T]):T	=
		T.concatAll1(items)
}

trait Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	// TODO in cats this is named combine
	def concat(a:F, b:F):F

	//------------------------------------------------------------------------------
	//## derived

	// TODO foldable add a Foldable1 typeclass
	def concatAll1(items:Nes[F]):F	=
		items.reduce(concat)(this)

	def foldMap1[S](items:Nes[S])(func:S=>F):F	=
		items.tail.foldLeft(func(items.head)) { (f, s) =>
			concat(f, func(s))
		}
}
