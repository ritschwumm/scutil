package scutil.lang

import scala.collection.IterableFactory

import scutil.lang.tc.*

object Nes {
	def pure[T](head:T):Nes[T]	=
		one(head)

	def one[T](head:T):Nes[T]	=
		Nes(head, Vector.empty)

	def of[T](head:T, tail:T*):Nes[T]	=
		Nes(head, tail.toVector)

	def fromSeq[T](it:Seq[T]):Option[Nes[T]]	=
		if it.nonEmpty
		then Some(unsafeFromSeq(it))
		else None

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def unsafeFromSeq[T](it:Seq[T]):Nes[T]	=
		Nes(it.head, it.tail)

	def unsafeFromIArray[T](it:IArray[T]):Nes[T]	=
		Nes(it.head, it.tail)

	// TODO collection improve performance
	def unsafeFromArray[T](it:Array[T]):Nes[T]	=
		unsafeFromSeq(it.toIndexedSeq)

	object Var {
		def apply[T](head:T, tail:T*):Nes[T]	=
			Nes(head, tail.toVector)

		def unapplySeq[T](nes:Nes[T]):Seq[T]	=
			nes.toVector
	}

	//------------------------------------------------------------------------------
	//## typeclass instances

	given TraversedMonad[Nes]	=
		new TraversedMonad[Nes] {
			override def map[A, B](it:Nes[A])(func:A=>B):Nes[B]				= it.map(func)
			override def pure[A](it:A):Nes[A]								= Nes.one(it)
			override def flatMap[A, B](it:Nes[A])(func:A=>Nes[B]):Nes[B]	= it.flatMap(func)
			override def traverse[G[_]:Applicative as AP, S, T](it:Nes[S])(func:S=>G[T]):G[Nes[T]]	=
				(it.tail.map(func).foldLeft(AP.map(func(it.head))(Nes.one[T]))) { (xs, x) =>
					AP.map2(xs, x)(_ :+ _)
				}
		}

	given [T] => Semigroup[Nes[T]]	=
		Semigroup.instance(_ ++ _)
}

final case class Nes[+T](head:T, tail:Seq[T]) {
	def last:T	=
		tail.lastOption.getOrElse(head)

	def init:Seq[T]	=
		if tail.nonEmpty
		then head +: tail.dropRight(1)
		else Vector(head)

	def size:Int	= tail.size + 1

	def containsIndex(index:Int):Boolean	=
		index >= 0 && index < size

	def lift(index:Int):Option[T]	=
		get(index)

	def get(index:Int):Option[T]	=
		if index == 0
		then Some(head)
		else tail.lift(index - 1)

	def count(pred:Predicate[T]):Int	=
		(if pred(head) then 1 else 0) +
		tail.count(pred)

	def forall(pred:Predicate[T]):Boolean	=
		pred(head) && tail.forall(pred)

	def exists(pred:Predicate[T]):Boolean	=
		pred(head) || tail.exists(pred)

	def drop(count:Int):Option[Nes[T]]	=
		Nes.fromSeq(toVector.drop(count))

	def take(count:Int):Option[Nes[T]]	=
		Nes.fromSeq(toVector.take(count))

	def dropWhile(pred:Predicate[T]):Option[Nes[T]]	=
		Nes.fromSeq(toVector.dropWhile(pred))

	def takeWhile(pred:Predicate[T]):Option[Nes[T]]	=
		Nes.fromSeq(toVector.takeWhile(pred))

	def tailNes:Option[Nes[T]]	=
		Nes.fromSeq(tail)

	def initNes:Option[Nes[T]]	=
		if tail.isEmpty
		then None
		else Some(Nes(head, tail.dropRight(1)))

	def map[U](func:T=>U):Nes[U]	=
		Nes(func(head), tail.map(func))

	// NOTE zipWith and map2 are different things for Iterables
	def map2[U, V](that:Nes[U])(func:(T, U)=>V):Nes[V]	=
		for {
			thisVal	<- this
			thatVal	<- that
		}
		yield func(thisVal, thatVal)

	def product[U](that:Nes[U]):Nes[(T, U)]	=
		map2(that)((_, _))

	def flatMap[U](func:T=>Nes[U]):Nes[U]	= {
		val Nes(h, t)	= func(head)
		val tt			= tail.flatMap { it => func(it).toSeq }
		Nes(h, t ++ tt)
	}

	def flatten[U](using ev:T <:< Nes[U]):Nes[U]	=
		flatMap(ev)

	def filter(pred:Predicate[T]):Option[Nes[T]]	=
		Nes.fromSeq(toSeq.filter(pred))

	def filterNot(pred:Predicate[T]):Option[Nes[T]]	=
		Nes.fromSeq(toSeq.filterNot(pred))

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def reverse:Nes[T]	=
		if tail.nonEmpty
		then Nes(tail.last, tail.init.reverse :+ head)
		else this

	def concat[U>:T](that:Nes[U]):Nes[U]	=
		Nes(this.head, (this.tail :+ that.head) ++ that.tail)

	def prepend[U>:T](item:U):Nes[U]	=
		Nes(item, toSeq)

	def append[U>:T](item:U):Nes[U]	=
		Nes(this.head, this.tail :+ item)

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def prependMany[U>:T](items:Seq[U]):Nes[U]	=
		if items.nonEmpty
		then Nes(items.head, items.tail ++ this.toVector)
		else this

	def appendMany[U>:T](items:Seq[U]):Nes[U]	=
		if items.nonEmpty
		then Nes(this.head, this.tail ++ items)
		else this

	inline def ++[U>:T](that:Nes[U]):Nes[U]	=
		this.concat(that)

	inline def :+[U>:T](item:U):Nes[U]	=
		this.append(item)

	inline def +:[U>:T](item:U):Nes[U]	=
		this.prepend(item)

	def updatedBy[U>:T](index:Int, func:U=>U):Option[Nes[U]]	=
		if containsIndex(index) then {
			Some(
				if index == 0
				then Nes(func(head), tail)
				else Nes(head, tail.updated(index-1, func(tail.apply(index-1))))
			)
		}
		else None

	def updatedAt[U>:T](index:Int, item:U):Option[Nes[U]]	=
		if containsIndex(index) then {
			Some(
				if index == 0
				then Nes(item, tail)
				else Nes(head, tail.updated(index-1, item))
			)
		}
		else None

	def zip[U](that:Nes[U]):Nes[(T, U)]	=
		Nes(
			(this.head, that.head),
			this.tail `zip` that.tail
		)

	// NOTE zipWith and map2 are different things for Iterables
	def zipWith[U, V](that:Nes[U])(func:(T, U)=>V):Nes[V]	=
		Nes(
			func(this.head, that.head),
			this.tail.zip(that.tail).map(func.tupled)
		)

	def zipWithIndex:Nes[(T, Int)]	=
		Nes(
			(this.head, 0),
			this.tail.zipWithIndex.map { (v, i) => (v, i+1) }
		)

	@deprecated("use combineAll1", "0.267.0")
	def reduce[U>:T:Semigroup as S]:U   =
		combineAll1

	// semigroups are associative, but not necessarily commutative, so the order does make a difference here - and we run from the left
	def combineAll1[U>:T:Semigroup as S]:U	=
		reduceLeft(S.combine)

	def reduceLeft[U>:T](func:(U, U)=>U):U	= {
		val iter	= tail.iterator
		var state:U	= head
		while iter.hasNext do {
			state	= func(state, iter.next())
		}
		state
	}

	def reduceRight[U>:T](func:(U, U)=>U):U	= {
		val iter	= tail.reverseIterator
		if (iter.hasNext) then {
			var state:U	= iter.next()
			while iter.hasNext do {
				state	= func(iter.next(), state)
			}
			func(state, head)
		}
		else head
	}

	def storeAt[U>:T](index:Int):Option[Store[U, Nes[U]]]	=
		get(index).map { item =>
			Store(
				item,
				(it:U) => {
					if index == 0
					then Nes(it, tail)
					else Nes(head, tail.updated(index-1, it))
				}
			)
		}

	def foreach(effect:Effect[T]):Unit	= {
		effect(head)
		tail.foreach(effect)
	}

	def toList:List[T]	=
		head :: tail.toList

	def toVector:Vector[T]	=
		head +: tail.toVector

	def toSeq:Seq[T]	=
		toVector

	def to[CC[_], U>:T](factory:IterableFactory[CC]):CC[U]	=
		factory.from(toVector)

	def mkString(separator:String):String	=
		toVector.mkString(separator)

	def mkString(prefix:String, separator:String, suffix:String):String	=
		toVector.mkString(prefix, separator, suffix)
}
