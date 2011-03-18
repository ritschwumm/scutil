package scutil

object IsInstanceOf {
	def apply[T](implicit mt:Manifest[T]):IsInstanceOf[T]	= new IsInstanceOf[T]
}

/**
typesafe pattern matching helper
T is the desired type, U the actual type

usage example:
val ListOfString	= TypedAs[List[String]]
foobar match { case ListOfString(strings) => ... }
*/
class IsInstanceOf[T](implicit mt:Manifest[T]) {
	def unapply[U](u:U)(implicit mu:Manifest[U]):Option[T] = {
		def sameArgTypes	= mt.typeArguments zip mu.typeArguments forall { case (at,au) => at >:> au }
		if (mt >:> mu && sameArgTypes)	Some(u.asInstanceOf[T])
		else							None
	}
}

/*
object Test {
	def main(args:Array[String]) {
		test(List(1,2,3))
		test(List("1", "2", "3"))
	}
	def test[T](value:T)(implicit mf:Manifest[T]) {
		val IsListOfInt	= IsInstanceOf[List[Int]]
		value match	{
			case IsListOfInt(list)	=> println("list of int")
			case x:List[_]			=> println("list of other")
			case _					=> println("other")
		}
	}
}
*/
