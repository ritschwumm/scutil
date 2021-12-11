package scutil.lang.tc

import scutil.lang.Nes

object SemigroupSyntax {
	implicit final class SemigroupSyntaxExt[T](peer:T)(using T:Semigroup[T]) {
		def combine(that:T):T	= T.combine(peer, that)
	}

	implicit final class NesMonoidSyntaxExt[T](peer:Nes[T])(using T:Semigroup[T]) {
		def combineAll1:T	= T.combineAll1(peer)
	}

	implicit final class NesMonoidSyntax2Ext[T](peer:Nes[T]) {
		def foldMap1[U](func:T=>U)(using T:Semigroup[U]):U	= T.foldMap1(peer)(func)
	}
}
