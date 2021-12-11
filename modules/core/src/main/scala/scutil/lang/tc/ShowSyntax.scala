package scutil.lang.tc

object ShowSyntax {
	implicit final class ShowValueSyntaxExt[T](peer:T)(using TC:Show[T]) {
		def show:String	= TC show peer
	}
}
