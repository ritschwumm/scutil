package scutil.lang.tc

import scutil.lang.Nes

object SemigroupSyntax extends SemigroupSyntax

trait SemigroupSyntax {
	implicit final class SemigroupSyntaxExt[T](peer:T)(implicit T:Semigroup[T]) {
		def concat(that:T):T	= T.concat(peer, that)
	}

	implicit final class NesMonoidSyntaxExt[T](peer:Nes[T])(implicit T:Semigroup[T]) {
		def concatAll1:T	= T.concatAll1(peer)
	}

	implicit final class NesMonoidSyntax2Ext[T](peer:Nes[T]) {
		def foldMap1[U](func:T=>U)(implicit T:Semigroup[U]):U	= T.foldMap1(peer)(func)
	}
}
