package scutil.lang.tc

object Monoid {
	def apply[F](implicit ev:Monoid[F]):Monoid[F]	= ev

	def instance[T](empty1:T, func:(T,T)=>T):Monoid[T]	=
		new Monoid[T] {
			def empty:T				= empty1
			def concat(a:T, b:T):T	= func(a, b)
		}

	//------------------------------------------------------------------------------

	def empty[T](implicit T:Monoid[T]):T	= T.empty

	def concatOf[T:Monoid](items:T*):T	=
		concatAll(items)

	def concatAll[T](items:Iterable[T])(implicit T:Monoid[T]):T	=
		T.concatAll(items)
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
			accu	= concat(accu, item)
			i		+= 1
		}
		accu
	}

	// TODO foldable add a Foldable typeclass
	def concatAll(items:Iterable[F]):F	=
		 items.foldLeft(empty)(concat)

	// TODO foldable add a Foldable typeclass
	def foldMap[S](items:Iterable[S])(func:S=>F):F	=
		 items.foldLeft(empty) { (f, s) =>
		 	concat(f, func(s))
		 }
}
