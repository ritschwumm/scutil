package scutil.diff

sealed trait Delta[+T]
final case class Include[+T](index:Int, elem:T)	extends Delta[T]
final case class Remove[+T](index:Int, old:T)		extends Delta[T] 
