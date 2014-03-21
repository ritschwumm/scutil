package scutil.lang

object Nes {
	def single[T](head:T):Nes[T]	=
			Nes(head, Vector.empty)
		
	def multi[T](head:T, tail:T*):Nes[T]	=
			Nes(head, tail)
		
	def fromSeq[T](it:Seq[T]):Option[Nes[T]]	=
			if (it.nonEmpty)	Some(Nes(it.head, it.tail))
			else				None
		
	object Var {
		def apply[T](head:T, tail:T*):Nes[T]	=
				Nes(head, tail)
			
		def unapplySeq[T](nes:Nes[T]):Option[Seq[T]]	=
				Some(nes.toVector)
	}
}

case class Nes[+T](head:T, tail:Seq[T]) {
	def foreach(effect:Effect[T]) {
		effect(head)
		tail foreach effect
	}
	
	def map[U](func:T=>U):Nes[U]	=
			Nes(func(head), tail map func)
		
	def flatMap[U](func:T=>Nes[U]):Nes[U]	= {
		val Nes(h, t)	= func(head)
		val tt			= tail flatMap { it => func(it).toSeq }
		Nes(h, t ++ tt)
	}
	
	def flatten[U](implicit ev:T=>Nes[U]):Nes[U]	=
			flatMap(ev)
	
	def filtered(pred:Predicate[T]):Option[Nes[T]]	=
			Nes fromSeq (toSeq filter pred)
		
	def reverse:Nes[T]	=
			if (tail.nonEmpty)	Nes(tail.last, tail.reverse :+ head)
			else				this
		
	def ++[U>:T](that:Nes[U]):Nes[U]	=
			Nes(this.head, (this.tail :+ that.head) ++ that.tail)
		
	def :+[U>:T](item:U):Nes[U]	=
			Nes(head, tail :+ item)
		
	def +:[U>:T](item:U):Nes[U]	=
			Nes(item, head +: tail)
		
	def zip[U](that:Nes[U]):Nes[(T,U)]	=
			Nes(
				(this.head, that.head),
				this.tail zip that.tail)
				
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
	
	def toList:List[T]	=
			head :: tail.toList
		
	def toVector:Seq[T]	=
			head +: tail.toVector
		
	def toSeq:Seq[T]	=
			toVector
}
