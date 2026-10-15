package scutil.lang.tc

object ShowSyntax {
	extension [T](peer:T)(using TC:Show[T]) {
		def show:String	= TC.show(peer)
	}
}
