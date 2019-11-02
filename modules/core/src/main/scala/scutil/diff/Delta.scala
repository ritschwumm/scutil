package scutil.diff

object Delta {
	final case class Include[+T](index:Int, elem:T)	extends Delta[T]
	final case class Remove[+T](index:Int, old:T)	extends Delta[T]
}

sealed trait Delta[+T]
