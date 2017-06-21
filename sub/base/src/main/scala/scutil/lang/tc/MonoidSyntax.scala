package scutil.lang.tc

object MonoidSyntax extends MonoidSyntax

trait MonoidSyntax {
	implicit class MonoidSyntaxExt[T](peer:T)(implicit SG:Monoid[T]) {
		def concat(that:T):T	= SG.concat(peer, that)
	}
	
	// TODO add foldable
}
