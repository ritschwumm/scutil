package scutil.diff

sealed trait Delta[+T]
case class Include[+T](index:Int, elem:T)	extends Delta[T]
case class Remove[+T](index:Int, old:T)		extends Delta[T] 
