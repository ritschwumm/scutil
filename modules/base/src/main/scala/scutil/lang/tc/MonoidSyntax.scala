package scutil.lang.tc

object MonoidSyntax extends MonoidSyntax

trait MonoidSyntax {
	implicit final class MonoidSyntaxExt[T](peer:T)(implicit SG:Monoid[T]) {
		def concat(that:T):T	= SG.concat(peer, that)
	}

	// TODO add a Foldable typeclass
	implicit final class IterableMonoidSyntaxExt[T](peer:Iterable[T])(implicit SG:Monoid[T]) {
		def concatAll:T	= Monoid concatAll peer
	}
}
