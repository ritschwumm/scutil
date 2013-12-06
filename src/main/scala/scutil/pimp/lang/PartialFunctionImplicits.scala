package scutil.pimp

import scutil.lang._

import scutil.lang.PFunction

object PartialFunctionImplicits extends PartialFunctionImplicits

trait PartialFunctionImplicits {
	implicit def toPartialFunctionExt[S,T](peer:PartialFunction[S,T])	= new PartialFunctionExt[S,T](peer)
}

final class PartialFunctionExt[S,T](peer:PartialFunction[S,T]) {
	def orDefault(value: =>T):S=>T	=
			orAlways(constant(value))
		
	def orAlways(func:S=>T):S=>T	= it =>
			if (peer isDefinedAt it)	peer(it) 
			else						func(it)
		
	def andThenFixed[U](that:PartialFunction[T,U]):PartialFunction[S,U]	=
			new PartialFunction[S,U] {
				def isDefinedAt(it:S):Boolean	= (peer isDefinedAt it) && (that isDefinedAt peer(it))
				def apply(it:S):U				= that(peer(it))
			}
	
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
