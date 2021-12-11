package scutil.lang.tc

object MonoidSyntax extends MonoidSyntax

trait MonoidSyntax {
	implicit final class MonoidSyntaxExt[T](peer:T)(using T:Monoid[T]) {
		def times(count:Int):T	= T.times(peer, count)
	}

	implicit final class IterableMonoidSyntaxExt[T](peer:Iterable[T])(using T:Monoid[T]) {
		def combineAll:T	= T.combineAll(peer)
	}

	implicit final class IterableMonoidSyntax2Ext[T](peer:Iterable[T]) {
		def foldMap[U](func:T=>U)(using U:Monoid[U]):U	= U.foldMap(peer)(func)
	}
}
