package scutil.lang.tc

import scutil.lang.Nes

object SemigroupSyntax {
	extension [T:Semigroup as T](peer:T) {
		def combine(that:T):T	= T.combine(peer, that)
	}

	extension [T:Semigroup as T](peer:Nes[T]) {
		def combineAll1:T	= T.combineAll1(peer)
	}

	extension [T](peer:Nes[T]) {
		def foldMap1[U:Semigroup as T](func:T=>U):U	= T.foldMap1(peer)(func)
	}
}
