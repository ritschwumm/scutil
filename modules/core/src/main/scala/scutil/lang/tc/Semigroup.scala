package scutil.lang.tc

import scutil.lang.Nes

object Semigroup {
	def apply[F](implicit ev:Semigroup[F]):Semigroup[F]	= ev

	def instance[T](func:(T,T)=>T):Semigroup[T]	=
		new Semigroup[T] {
			def combine(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def combine[T](a:T, b:T)(implicit T:Semigroup[T]):T	=
		T.combine(a, b)

	def combineOf1[T:Semigroup](x:T, xs:T*)	=
		combineAll1(Nes(x, xs))

	def combineAll1[T](items:Nes[T])(implicit T:Semigroup[T]):T	=
		T.combineAll1(items)
}

trait Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	def combine(a:F, b:F):F

	//------------------------------------------------------------------------------
	//## derived

	// TODO foldable add a Foldable1 typeclass
	def combineAll1(items:Nes[F]):F	=
		items.reduce(combine)(this)

	def foldMap1[S](items:Nes[S])(func:S=>F):F	=
		items.tail.foldLeft(func(items.head)) { (f, s) =>
			combine(f, func(s))
		}
}
