package scutil.lang.tc

import scutil.lang.Nes

import scutil.lang.extension.PFunctionExtensions.*

object Semigroup extends SemigroupLow {
	def apply[F](using ev:Semigroup[F]):Semigroup[F]	= ev

	def instance[T](func:(T,T)=>T):Semigroup[T]	=
		new Semigroup[T] {
			def combine(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def combine[T](a:T, b:T)(using T:Semigroup[T]):T	=
		T.combine(a, b)

	def combineOf1[T:Semigroup](x:T, xs:T*)	=
		combineAll1(Nes(x, xs))

	def combineAll1[T](items:Nes[T])(using T:Semigroup[T]):T	=
		T.combineAll1(items)

	//------------------------------------------------------------------------------

	// TODO questionable
	given [S,T]:Semigroup[S=>Option[T]]	=
		Semigroup instance (_ orElse _)

	/*
	// TODO does this make sense without further constraints on T?
	given [S,T]:Semigroup[Either[S,T]]	=
		Semigroup instance { (a,b) =>
			a match {
				case Left(_)	=> b
				case Right(_)	=> a
			}
		}
	*/

	/*
	// NOTE already done with PFunctionSemigroup
	given [T]:Semigroup[T=>Option[T]]	=
		Semigroup instance (_ andThenFixed _)
	*/

}

trait SemigroupLow {
	// TODO dotty is this really necessary? why doesn't inheritance provide us with an instance?
	given[F](using F:Monoid[F]):Semigroup[F]	= F
}

trait Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	def combine(a:F, b:F):F

	//------------------------------------------------------------------------------
	//## derived

	// TODO foldable add a Foldable1 typeclass
	def combineAll1(items:Nes[F]):F	=
		items.reduce(combine)(using this)

	def foldMap1[S](items:Nes[S])(func:S=>F):F	=
		items.tail.foldLeft(func(items.head)) { (f, s) =>
			combine(f, func(s))
		}
}

