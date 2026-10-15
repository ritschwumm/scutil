package scutil.lang.tc

object ShowSyntax {
	extension [T:Show as TC](peer:T) {
		def show:String	= TC.show(peer)
	}
}
