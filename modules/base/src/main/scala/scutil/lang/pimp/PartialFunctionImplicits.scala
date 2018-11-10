package scutil.lang.pimp

import scutil.lang._

import scutil.lang.PFunction

object PartialFunctionImplicits extends PartialFunctionImplicits

trait PartialFunctionImplicits {
	implicit final class PartialFunctionExt[S,T](peer:PartialFunction[S,T]) {
		def toPFunction:PFunction[S,T]	=
				peer.lift

		def toExtractor:Extractor[S,T]	=
				Extractor(peer.lift)
	}
}
