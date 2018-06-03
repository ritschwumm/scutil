package scutil.lang.pimp

import scutil.lang._

import scutil.lang.PFunction

object PartialFunctionImplicits extends PartialFunctionImplicits

trait PartialFunctionImplicits {
	implicit final class PartialFunctionExt[S,T](peer:PartialFunction[S,T]) {
		@deprecated("use a PFunction", "0.142.0")
		def orDefault(value: =>T):S=>T	=
				orAlways(constant(value))
			
		@deprecated("use a PFunction", "0.142.0")
		def orAlways(func:S=>T):S=>T	=
				it => peer applyOrElse (it, func)
			
		/** symbolic alias for andThenFixed */
		@deprecated("use a PFunction", "0.142.0")
		def >=>[U](that:PartialFunction[T,U]):PartialFunction[S,U]	=
				this andThenFixed that
			
		/** symbolic alias for composeFixed */
		@deprecated("use a PFunction", "0.142.0")
		def <=<[R](that:PartialFunction[R,S]):PartialFunction[R,T]	=
				this composeFixed that
			
		@deprecated("use a PFunction", "0.142.0")
		def andThenFixed[U](that:PartialFunction[T,U]):PartialFunction[S,U]	=
				new PartialFunction[S,U] {
					def isDefinedAt(it:S):Boolean	= (peer isDefinedAt it) && (that isDefinedAt peer(it))
					def apply(it:S):U				= that(peer(it))
				}
		
		@deprecated("use a PFunction", "0.142.0")
		def composeFixed[R](that:PartialFunction[R,S]):PartialFunction[R,T]	=
				new PartialFunction[R,T] {
					def isDefinedAt(it:R):Boolean	= (that isDefinedAt it) && (peer isDefinedAt that(it))
					def apply(it:R):T				= peer(that(it))
				}
		
		def toPFunction:PFunction[S,T]	=
				peer.lift
			
		def toExtractor:Extractor[S,T]	=
				Extractor(peer.lift)
	}
}
