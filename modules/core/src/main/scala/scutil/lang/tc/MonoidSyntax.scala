package scutil.lang.tc

object MonoidSyntax {
	extension [T](peer:T)(using T:Monoid[T]) {
		def times(count:Int):T	= T.times(peer, count)
	}

	extension [T](peer:Iterable[T])(using T:Monoid[T]) {
		def combineAll:T	= T.combineAll(peer)
	}

	extension [T](peer:Iterable[T]) {
		def foldMap[U](func:T=>U)(using U:Monoid[U]):U	= U.foldMap(peer)(func)
	}
}
