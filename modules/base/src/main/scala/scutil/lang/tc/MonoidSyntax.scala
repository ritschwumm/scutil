package scutil.lang.tc

object MonoidSyntax extends MonoidSyntax

trait MonoidSyntax {
	implicit final class MonoidSyntaxExt[T](peer:T)(implicit SG:Monoid[T]) {
		def times(count:Int):T	= SG.times(peer, count)
	}

	// TODO add a Foldable typeclass
	implicit final class IterableMonoidSyntaxExt[T](peer:Iterable[T])(implicit SG:Monoid[T]) {
		def concatAll:T	= Monoid concatAll peer
	}
}
