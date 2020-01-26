package scutil.lang

import scala.collection.IterableFactory

import scutil.lang.tc._

object Nes {
	def single[T](head:T):Nes[T]	=
		Nes(head, Vector.empty)

	def multi[T](head:T, tail:T*):Nes[T]	=
		Nes(head, tail.toVector)

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def fromSeq[T](it:Seq[T]):Option[Nes[T]]	=
		if (it.nonEmpty)	Some(Nes(it.head, it.tail))
		else				None

	object Var {
		def apply[T](head:T, tail:T*):Nes[T]	=
			Nes(head, tail.toVector)

		def unapplySeq[T](nes:Nes[T]):Option[Seq[T]]	=
			Some(nes.toVector)
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit def NesTraversedMonad:TraversedMonad[Nes]	=
		new TraversedMonad[Nes] {
			override def map[A,B](it:Nes[A])(func:A=>B):Nes[B]			= it map func
			override def pure[A](it:A):Nes[A]							= Nes single it
			override def flatMap[A,B](it:Nes[A])(func:A=>Nes[B]):Nes[B]	= it flatMap func
			override def traverse[G[_],S,T](it:Nes[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[Nes[T]]	=
				((it.tail map func) foldLeft ((AP map  func(it.head))(Nes.single[T]))) { (xs, x) =>
					(AP combine (xs, x))(_ :+ _)
				}
		}

	implicit def NesSemigroup[T]:Semigroup[Nes[T]]	=
		Semigroup instance (_ ++ _)
}

final case class Nes[+T](head:T, tail:Seq[T]) {
	def last:T	=
		tail.lastOption getOrElse head

	def init:Seq[T]	=
		if (tail.nonEmpty)	head +: (tail dropRight 1)
		else				Vector(head)

	def size:Int	= tail.size + 1

	def containsIndex(index:Int):Boolean	=
		index >= 0 && index < size

	def lift(index:Int):Option[T]	=
		get(index)

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
		Nes fromSeq (toVector drop count)

	def take(count:Int):Option[Nes[T]]	=
		Nes fromSeq (toVector take count)

	def dropWhile(pred:Predicate[T]):Option[Nes[T]]	=
		Nes fromSeq (toVector dropWhile pred)

	def takeWhile(pred:Predicate[T]):Option[Nes[T]]	=
		Nes fromSeq (toVector takeWhile pred)

	def tailNes:Option[Nes[T]]	=
		Nes fromSeq tail

	def initNes:Option[Nes[T]]	=
		if (tail.isEmpty)	None
		else				Some(Nes(head, tail dropRight 1))

	def map[U](func:T=>U):Nes[U]	=
		Nes(func(head), tail map func)

	def flatMap[U](func:T=>Nes[U]):Nes[U]	= {
		val Nes(h, t)	= func(head)
		val tt			= tail flatMap { it => func(it).toSeq }
		Nes(h, t ++ tt)
	}

	def flatten[U](implicit ev:T=>Nes[U]):Nes[U]	=
		flatMap(ev)

	def filter(pred:Predicate[T]):Option[Nes[T]]	=
		Nes fromSeq (toSeq filter pred)

	def filterNot(pred:Predicate[T]):Option[Nes[T]]	=
		Nes fromSeq (toSeq filterNot pred)

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def reverse:Nes[T]	=
		if (tail.nonEmpty)	Nes(tail.last, tail.init.reverse :+ head)
		else				this

	def withReverse[U>:T](func:Endo[Nes[U]]):Nes[U]	=
		func(reverse).reverse

	def concat[U>:T](that:Nes[U]):Nes[U]	=
		Nes(this.head, (this.tail :+ that.head) ++ that.tail)

	def prepend[U>:T](item:U):Nes[U]	=
		Nes(item, toSeq)

	def append[U>:T](item:U):Nes[U]	=
		Nes(this.head, this.tail :+ item)

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def prependMany[U>:T](items:Seq[U]):Nes[U]	=
		if (items.nonEmpty)	Nes(items.head, items.tail ++ this.toVector)
		else				this

	def appendMany[U>:T](items:Seq[U]):Nes[U]	=
		if (items.nonEmpty)	Nes(this.head, this.tail ++ items)
		else				this

	@inline
	def ++[U>:T](that:Nes[U]):Nes[U]	=
		this concat that

	@inline
	def :+[U>:T](item:U):Nes[U]	=
		this append item

	@inline
	def +:[U>:T](item:U):Nes[U]	=
			this prepend item

	def updatedBy[U>:T](index:Int, func:Endo[U]):Option[Nes[U]]	=
		if (containsIndex(index)) {
			Some(
				if (index == 0)	Nes(func(head), tail)
				else			Nes(head, tail updated (index-1, func(tail.apply(index-1))))
			)
		}
		else None

	def updatedAt[U>:T](index:Int, item:U):Option[Nes[U]]	=
		if (containsIndex(index)) {
			Some(
				if (index == 0)	Nes(item, tail)
				else			Nes(head, tail updated (index-1, item))
			)
		}
		else None

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

	def reduce[U>:T](func:(U,U)=>U)(implicit S:Semigroup[U]):U	=
		reduceWith(S.concat)

	def reduceWith[U>:T](func:(U,U)=>U):U	= {
		var state:U	= head
		tail foreach { it =>
			state	= func(state, it)
		}
		state
	}

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

	def foreach(effect:Effect[T]):Unit	= {
		effect(head)
		tail foreach effect
	}

	def toList:List[T]	=
		head :: tail.toList

	def toVector:Vector[T]	=
		head +: tail.toVector

	def toSeq:Seq[T]	=
		toVector

	def to[CC[_],U>:T](factory:IterableFactory[CC]):CC[U]	=
		factory from toVector

	def mkString(separator:String):String	=
		toVector mkString separator

	def mkString(prefix:String, separator:String, suffix:String):String	=
		toVector mkString (prefix, separator, suffix)
}
