package scutil.lang.extension

import scutil.lang.*

object PartialFunctionExtensions {
	extension [S,T](peer:PartialFunction[S,T]) {
		def toPFunction:S=>Option[T]	= peer.lift
		def toExtractor:Extractor[S,T]	= Extractor(peer.lift)
	}
}
