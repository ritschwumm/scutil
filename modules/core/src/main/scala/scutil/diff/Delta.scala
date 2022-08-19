package scutil.diff

enum Delta[+T] {
	case Include[+T](index:Int, elem:T)	extends Delta[T]
	case Remove[+T](index:Int, old:T)	extends Delta[T]
}
