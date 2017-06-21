package scutil.collection

import scutil.lang.ISeq
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
							(xs, x) => (AP combine (xs, x))(_ :+ _)
						}
			}
			
	implicit def VectorMonoid[T]:Monoid[Vector[T]]	=
			Monoid instance (Vector.empty, _ ++ _)
			
	implicit def ListTraversedMonad:TraversedMonad[List]	=
			new TraversedMonad[List] {
				override def map[A,B](it:List[A])(func:A=>B):List[B]			= it map func
				override def pure[A](it:A):List[A]								= List(it)
				override def flatMap[A,B](it:List[A])(func:A=>List[B]):List[B]	= it flatMap func
				override def traverse[G[_],S,T](it:List[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[List[T]]	=
						(it map func foldLeft (AP pure List.empty[T])) {
							(xs, x) => (AP combine (xs, x))(_ :+ _)
						}
			}
			
	implicit def ListMonoid[T]:Monoid[List[T]]	=
			Monoid instance (Nil, _ ++ _)
		
	// no Functor[Set]
	
	implicit def SetMonoid[T]:Monoid[Set[T]]	=
			Monoid instance (Set.empty, _ ++ _)
		
	// no Functor[Map]

	implicit def MapMonoid[K,V]:Monoid[Map[K,V]]	=
			Monoid instance (Map.empty, _ ++ _)
}

trait instancesLow {
	implicit def ISeqTraversedMonad:TraversedMonad[ISeq]	=
			new TraversedMonad[ISeq] {
				override def map[A,B](it:ISeq[A])(func:A=>B):ISeq[B]			= it map func
				override def pure[A](it:A):ISeq[A]								= ISeq(it)
				override def flatMap[A,B](it:ISeq[A])(func:A=>ISeq[B]):ISeq[B]	= it flatMap func
				override def traverse[G[_],S,T](it:ISeq[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[ISeq[T]]	=
						(it map func foldLeft (AP pure ((Vector.empty[T]):ISeq[T]))) {
							(xs, x) => (AP combine (xs, x))(_ :+ _)
						}
			}
			
	implicit def ISeqMonoid[T]:Monoid[ISeq[T]]	=
			Monoid instance (ISeq.empty, _ ++ _)
}
