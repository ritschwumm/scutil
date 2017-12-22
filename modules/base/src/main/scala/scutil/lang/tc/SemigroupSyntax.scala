package scutil.lang.tc

object SemigroupSyntax extends SemigroupSyntax

trait SemigroupSyntax {
	implicit class SemigroupSyntaxExt[T](peer:T)(implicit SG:Semigroup[T]) {
		def concat(that:T):T	= SG.concat(peer, that)
	}
}