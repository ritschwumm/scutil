package scutil.collection

import scutil.lang.tc._

object instances extends instances

trait instances extends instancesLow {
	implicit def VectorTraversedMonad:TraversedMonad[Vector]	=
		new TraversedMonad[Vector] {
			override def map[A,B](it:Vector[A])(func:A=>B):Vector[B]				= it map func
			override def pure[A](it:A):Vector[A]									= Vector(it)
			override def flatMap[A,B](it:Vector[A])(func:A=>Vector[B]):Vector[B]	= it flatMap func
			override def traverse[G[_],S,T](it:Vector[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[Vector[T]]	=
				(it map func foldLeft (AP pure Vector.empty[T])) {
					(xs, x) => AP.map2(xs, x)(_ :+ _)
				}
		}

	implicit def VectorMonoid[T]:Monoid[Vector[T]]	=
		Monoid.instance(Vector.empty, _ ++ _)

	implicit def ListTraversedMonad:TraversedMonad[List]	=
		new TraversedMonad[List] {
			override def map[A,B](it:List[A])(func:A=>B):List[B]			= it map func
			override def pure[A](it:A):List[A]								= List(it)
			override def flatMap[A,B](it:List[A])(func:A=>List[B]):List[B]	= it flatMap func
			override def traverse[G[_],S,T](it:List[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[List[T]]	= {
				val mapped		= it map func
				val empty		= AP pure List.empty[T]
				val reversed	=
					mapped.foldLeft(empty) { (xs, x) =>
						AP.map2(xs, x) { (a,b) =>
							// builds in reverse order because appending to a list is too expensive
							b :: a
						}
					}
				// reverse the effect of building in reverse
				AP.map(reversed)(_.reverse)
			}
		}

	implicit def ListMonoid[T]:Monoid[List[T]]	=
		Monoid.instance(Nil, _ ++ _)

	// no Functor[Set]

	implicit def SetMonoid[T]:Monoid[Set[T]]	=
		Monoid.instance(Set.empty, _ ++ _)

	// no Functor[Map]

	implicit def MapMonoid[K,V]:Monoid[Map[K,V]]	=
		Monoid.instance(Map.empty, _ ++ _)
}

trait instancesLow {
	implicit def SeqTraversedMonad:TraversedMonad[Seq]	=
		new TraversedMonad[Seq] {
			override def map[A,B](it:Seq[A])(func:A=>B):Seq[B]			= it map func
			override def pure[A](it:A):Seq[A]							= Seq(it)
			override def flatMap[A,B](it:Seq[A])(func:A=>Seq[B]):Seq[B]	= it flatMap func
			override def traverse[G[_],S,T](it:Seq[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[Seq[T]]	=
				(it map func foldLeft (AP pure ((Vector.empty[T]):Seq[T]))) {
					(xs, x) => AP.map2(xs, x)(_ :+ _)
				}
		}

	implicit def SeqMonoid[T]:Monoid[Seq[T]]	=
		Monoid.instance(Seq.empty, _ ++ _)
}
