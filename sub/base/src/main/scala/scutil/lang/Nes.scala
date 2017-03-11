package scutil.lang

import scutil.lang.tc._

object Nes extends NesInstances {
	def single[T](head:T):Nes[T]	=
			Nes(head, Vector.empty)
		
	def multi[T](head:T, tail:T*):Nes[T]	=
			Nes(head, tail.toVector)
		
	def fromISeq[T](it:ISeq[T]):Option[Nes[T]]	=
			if (it.nonEmpty)	Some(Nes(it.head, it.tail))
			else				None
		
	object Var {
		def apply[T](head:T, tail:T*):Nes[T]	=
				Nes(head, tail.toVector)
			
		def unapplySeq[T](nes:Nes[T]):Option[ISeq[T]]	=
				Some(nes.toVector)
	}
}

final case class Nes[+T](head:T, tail:ISeq[T]) {
	def last:T	=
			if (tail.nonEmpty)	tail.last
			else				head
			
	def init:ISeq[T]	=
			if (tail.nonEmpty)	head +: tail.init
			else				Vector(head)
			
	def size:Int	= tail.size + 1
	
	def containsIndex(index:Int):Boolean	=
			index >= 0 && index < size
		
	def get(index:Int):Option[T]	=
			if (index == 0)	Some(head)
			else			tail lift (index - 1)
		
	def count(pred:Predicate[T]):Int	=
			(if (pred(head)) 1 else 0) +
			(tail count pred)
			
	def forall(pred:Predicate[T]):Boolean	=
			pred(head) && (tail forall pred)
	
	def exists(pred:Predicate[T]):Boolean	=
			pred(head) || (tail exists pred)
	
	def drop(count:Int):Option[Nes[T]]	=
			Nes fromISeq (toVector drop count)
			
	def take(count:Int):Option[Nes[T]]	=
			Nes fromISeq (toVector take count)
			
	def dropWhile(pred:Predicate[T]):Option[Nes[T]]	=
			Nes fromISeq (toVector dropWhile pred)
		
	def takeWhile(pred:Predicate[T]):Option[Nes[T]]	=
			Nes fromISeq (toVector takeWhile pred)
		
	def tailNes:Option[Nes[T]]	=
			Nes fromISeq tail
	
	def initNes:Option[Nes[T]]	=
			if (tail.isEmpty)	None
			else				Some(Nes(head, tail.init))
	
	def map[U](func:T=>U):Nes[U]	=
			Nes(func(head), tail map func)
		
	def flatMap[U](func:T=>Nes[U]):Nes[U]	= {
		val Nes(h, t)	= func(head)
		val tt			= tail flatMap { it => func(it).toISeq }
		Nes(h, t ++ tt)
	}
	
	def flatten[U](implicit ev:T=>Nes[U]):Nes[U]	=
			flatMap(ev)
	
	def filter(pred:Predicate[T]):Option[Nes[T]]	=
			Nes fromISeq (toISeq filter pred)
		
	def filterNot(pred:Predicate[T]):Option[Nes[T]]	=
			Nes fromISeq (toISeq filterNot pred)
		
	def reverse:Nes[T]	=
			if (tail.nonEmpty)	Nes(tail.last, tail.init.reverse :+ head)
			else				this
		
	def withReverse[U>:T](func:Endo[Nes[U]]):Nes[U]	=
			func(reverse).reverse
		
	def concat[U>:T](that:Nes[U]):Nes[U]	=
			Nes(this.head, (this.tail :+ that.head) ++ that.tail)
	
	def prepend[U>:T](item:U):Nes[U]	=
			Nes(item, toISeq)
		
	def append[U>:T](item:U):Nes[U]	=
			Nes(this.head, this.tail :+ item)
		
	@inline
	def ++[U>:T](that:Nes[U]):Nes[U]	=
			this concat that
		
	@inline
	def :+[U>:T](item:U):Nes[U]	=
			this append item
		
	@inline
	def +:[U>:T](item:U):Nes[U]	=
			this prepend item
		
	def zip[U](that:Nes[U]):Nes[(T,U)]	=
			Nes(
				(this.head, that.head),
				this.tail zip that.tail
			)
				
	def zipWith[U,V](that:Nes[U])(func:(T,U)=>V):Nes[V]	=
			Nes(
				func(this.head, that.head),
				(this.tail zip that.tail) map func.tupled
			)
				
	def zipWithIndex:Nes[(T,Int)]	=
			Nes(
				(this.head, 0),
				this.tail.zipWithIndex map { case (v,i) => (v,i+1) }
			)
		
	def storeAt[U>:T](index:Int):Option[Store[Nes[U],U]]	=
			get(index) map { item =>
				Store(
					item,
					(it:U) => {
						if (index == 0)	Nes(it, tail)
						else			Nes(head, tail updated (index-1, it))
					}
				)
			}
	
	def foreach(effect:Effect[T]) {
		effect(head)
		tail foreach effect
	}
	
	def toList:List[T]	=
			head :: tail.toList
		
	def toVector:Vector[T]	=
			head +: tail.toVector
		
	def toISeq:ISeq[T]	=
			toVector
}

/** helper to allow easy construction and pattern matching */
object VarNes {
	def apply[T](head:T, tail:T*):Nes[T]			= Nes(head, tail.toVector)
	def unapplySeq[T](it:Nes[T]):Option[ISeq[T]]	= Some(it.toVector)
}

trait NesInstances {
	implicit def NesMonad:Monad[Nes]	=
			new Monad[Nes] {
				override def pure[A](it:A):Nes[A]							= Nes single it
				override def map[A,B](it:Nes[A])(func:A=>B):Nes[B]			= it map func
				override def flatMap[A,B](it:Nes[A])(func:A=>Nes[B]):Nes[B]	= it flatMap func
			}
			
	implicit def NesSemigroup[T]:Semigroup[Nes[T]]	=
			Semigroup by (_ ++ _)
}
