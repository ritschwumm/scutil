package scutil.ext

import scutil.lang.PFunction

object PartialFunctionImplicits extends PartialFunctionImplicits

trait PartialFunctionImplicits {
	implicit def toPartialFunctionExt[S,T](delegate:PartialFunction[S,T])	= new PartialFunctionExt[S,T](delegate)
}

final class PartialFunctionExt[S,T](delegate:PartialFunction[S,T]) {
	def orAlways(func:S=>T):S=>T	= it =>
			if (delegate isDefinedAt it)	delegate(it) 
			else							func(it)
		
	def andThenFixed[U](that:PartialFunction[T,U]):PartialFunction[S,U]	= new PartialFunction[S,U] {
		def isDefinedAt(it:S):Boolean	= (delegate isDefinedAt it) && (that isDefinedAt delegate(it))
		def apply(it:S):U				= that(delegate(it))
	}
	
	def composeFixed[R](that:PartialFunction[R,S]):PartialFunction[R,T]	= new PartialFunction[R,T] {
		def isDefinedAt(it:R):Boolean	= (that isDefinedAt it) && (delegate isDefinedAt that(it))
		def apply(it:R):T				= delegate(that(it))
	}
	
	def toPFunction:PFunction[S,T]	=
			delegate.lift
}
