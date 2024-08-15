package scutil.lang.tc

import scutil.lang.extension.OptionExtensions.*

object Monoid extends MonoidLow {
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

	//------------------------------------------------------------------------------

	// TODO tc generalize to any iterable and remove SeqMonoid

	given VectorMonoid[T]:Monoid[Vector[T]]	=
		Monoid.instance(Vector.empty, _ ++ _)

	given ListMonoid[T]:Monoid[List[T]]	=
		Monoid.instance(Nil, _ ++ _)

	given SetMonoid[T]:Monoid[Set[T]]	=
		Monoid.instance(Set.empty, _ ++ _)

	given UnitMonoid:Monoid[Unit]	=
		Monoid.instance((), (_,_)=>())

	given StringMonoid:Monoid[String]	=
		Monoid.instance("", _ + _)

	given PairMonoid[T1,T2](using T1:Monoid[T1], T2:Monoid[T2]):Monoid[(T1,T2)]	=
		Monoid.instance(
			(T1.empty, T2.empty),
			(a, b) => (
				T1.combine(a._1, b._1),
				T2.combine(a._2, b._2)
			)
		)

	//-----------------------------------------------------------------------------

	// TODO tc add more alternatives here

	def MapLastMonoid[K,V]:Monoid[Map[K,V]]	=
		Monoid.instance(Map.empty, _ ++ _)

	//-----------------------------------------------------------------------------

	// this is the one used in haskell
	def OptionMergeMonoid[T](using S:Semigroup[T]):Monoid[Option[T]]	=
		Monoid.instance(
			None,
			(a,b) => a.oneOrTwo(b)(S.combine)
		)

	def OptionBothMonoid[T](using S:Semigroup[T]):Monoid[Option[T]]	=
		Monoid.instance(
			None,
			(a,b) => (a,b) match {
				case (Some(a), Some(b))	=> Some(S.combine(a, b))
				case _					=> None
			}
		)

	def OptionFirstMonoid[T]:Monoid[Option[T]]	=
		Monoid.instance(
			None,
			(a,b) => a `orElse` b
		)

	def OptionLastMonoid[T]:Monoid[Option[T]]	=
		Monoid.instance(
			None,
			(a,b) => b `orElse` a
		)

	//-----------------------------------------------------------------------------

	def EndoMonoid[T]:Monoid[T=>T]	=
		Monoid.instance(identity, _ `andThen` _)

	def KleisliMonoid[S,T](using M:Monoid[T]):Monoid[S=>T]	=
		Monoid.instance(
						(s) => Monoid.empty[T],
			(a,b) =>	(s) => M.combine(a(s), b(s)),
		)
}

trait MonoidLow {
	given SeqMonoid[T]:Monoid[Seq[T]]	=
		Monoid.instance(Seq.empty, _ ++ _)
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
