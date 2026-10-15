package scutil.lang.tc

object MonoidSyntax {
	extension [T:Monoid as T](peer:T) {
		def times(count:Int):T	= T.times(peer, count)
	}

	extension [T:Monoid as T](peer:Iterable[T]) {
		def combineAll:T	= T.combineAll(peer)
	}

	extension [T](peer:Iterable[T]) {
		def foldMap[U:Monoid as U](func:T=>U):U	= U.foldMap(peer)(func)
	}
}
