package scutil.lang.tc

import scutil.lang.Nes

object SemigroupSyntax extends SemigroupSyntax

trait SemigroupSyntax {
	implicit final class SemigroupSyntaxExt[T](peer:T)(implicit SG:Semigroup[T]) {
		def concat(that:T):T	= SG.concat(peer, that)
	}

	// TODO add a Foldable1 typeclass
	implicit final class NesMonoidSyntaxExt[T](peer:Nes[T])(implicit SG:Semigroup[T]) {
		def concatAll1:T	= Semigroup concatAll1 peer
	}
}
