package scutil.ext

object OrderingImplicits extends OrderingImplicits

trait OrderingImplicits {
	implicit def toOrderingExt[T](delegate:Ordering[T]) = new OrderingExt[T](delegate)
}

final class OrderingExt[T](delegate:Ordering[T]) {
	/** alternative to the implicit (Ordering[T],Ordering[T]) => Ordering[T] in Ordering.Implicits */
	def orElse[U<:T](that:Ordering[U]):Ordering[U]	= new Ordering[U] {
		def compare(x:U, y:U):Int	= {
			val	high	= delegate compare (x,y)
			if (high != 0) high else that compare (x,y)
		} 
	}
}
