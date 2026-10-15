package scutil.lang.tc

import scutil.lang.Nes

object SemigroupSyntax {
	extension [T](peer:T)(using T:Semigroup[T]) {
		def combine(that:T):T	= T.combine(peer, that)
	}

	extension [T](peer:Nes[T])(using T:Semigroup[T]) {
		def combineAll1:T	= T.combineAll1(peer)
	}

	extension [T](peer:Nes[T]) {
		def foldMap1[U](func:T=>U)(using T:Semigroup[U]):U	= T.foldMap1(peer)(func)
	}
}
