package scutil.lang.tc

import scutil.lang.Nes

import scutil.lang.extension.OptionExtensions.*
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
		Semigroup.instance(_ `orElse` _)

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
		Semigroup instance (_ `andThenFixed` _)
	*/

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

trait SemigroupLow {
	given SeqMonoid[T]:Monoid[Seq[T]]	=
		Monoid.instance(Seq.empty, _ ++ _)
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
