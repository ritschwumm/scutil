package scutil.collection

import scutil.lang.ISeq
import scutil.lang.tc._

object instances extends instances

trait instances extends instancesLow {
	implicit def VectorMonad:Monad[Vector]	=
			new Monad[Vector] {
				override def pure[A](it:A):Vector[A]									= Vector(it)
				override def map[A,B](it:Vector[A])(func:A=>B):Vector[B]				= it map func
				override def flatMap[A,B](it:Vector[A])(func:A=>Vector[B]):Vector[B]	= it flatMap func
			}
			
	implicit def VectorMonoid[T]:Monoid[Vector[T]]	=
			Monoid by (Vector.empty, _ ++ _)
			
	implicit def ListMonad:Monad[List]	=
			new Monad[List] {
				override def pure[A](it:A):List[A]								= List(it)
				override def map[A,B](it:List[A])(func:A=>B):List[B]			= it map func
				override def flatMap[A,B](it:List[A])(func:A=>List[B]):List[B]	= it flatMap func
			}
			
	implicit def ListMonoid[T]:Monoid[List[T]]	=
			Monoid by (Nil, _ ++ _)
		
	implicit def SetMonoid[T]:Monoid[Set[T]]	=
			Monoid by (Set.empty, _ ++ _)
		
	// no Functor[Set]
		
	implicit def MapMonoid[K,V]:Monoid[Map[K,V]]	=
			Monoid by (Map.empty, _ ++ _)
		
	// no Functor[Map]
}

trait instancesLow {
	implicit def ISeqMonad:Monad[ISeq]	=
			new Monad[ISeq] {
				override def pure[A](it:A):ISeq[A]								= ISeq(it)
				override def map[A,B](it:ISeq[A])(func:A=>B):ISeq[B]			= it map func
				override def flatMap[A,B](it:ISeq[A])(func:A=>ISeq[B]):ISeq[B]	= it flatMap func
			}
			
	implicit def ISeqMonoid[T]:Monoid[ISeq[T]]	=
			Monoid by (ISeq.empty, _ ++ _)
	
}
