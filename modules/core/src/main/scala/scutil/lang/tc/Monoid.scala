package scutil.lang.tc

object Monoid {
	def apply[F](using ev:Monoid[F]):Monoid[F]	= ev

	def instance[T](empty1:T, func:(T,T)=>T):Monoid[T]	=
		new Monoid[T] {
			def empty:T				= empty1
			def combine(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def empty[T](using T:Monoid[T]):T	= T.empty

	def combineOf[T:Monoid](items:T*):T	=
		combineAll(items)

	def combineAll[T](items:Iterable[T])(using T:Monoid[T]):T	=
		T.combineAll(items)
}

trait Monoid[F] extends Semigroup[F] {
	//------------------------------------------------------------------------------
	//## own

	def empty:F

	//------------------------------------------------------------------------------
	//## derived

	def times(item:F, count:Int):F	= {
		var accu	= empty
		var i		= 0
		while (i < count) {
			accu	= combine(accu, item)
			i		+= 1
		}
		accu
	}

	// TODO foldable add a Foldable typeclass
	def combineAll(items:Iterable[F]):F	=
		items.foldLeft(empty)(combine)

	// TODO foldable add a Foldable typeclass
	def foldMap[S](items:Iterable[S])(func:S=>F):F	=
		items.foldLeft(empty) { (f, s) =>
			combine(f, func(s))
		}
}
