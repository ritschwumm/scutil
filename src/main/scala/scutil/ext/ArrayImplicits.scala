package scutil.ext

import scala.collection.immutable

object ArrayImplicits extends ArrayImplicits

trait ArrayImplicits {
	implicit def toArrayExt[T](delegate:Array[T])	= new ArrayExt(delegate)
}

final class ArrayExt[T](delegate:Array[T]) {
	def toVector:Vector[T]	= Vector(delegate:_*)
}
