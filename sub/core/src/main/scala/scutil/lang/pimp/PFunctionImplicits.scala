package scutil.lang.pimp

import scutil.lang._

object PFunctionImplicits extends PFunctionImplicits

trait PFunctionImplicits {
	implicit def toPFunctionExt[S,T](peer:PFunction[S,T])	= new PFunctionExt[S,T](peer)
}

final class PFunctionExt[S,T](peer:PFunction[S,T]) {
	def applyOrElse(it:S, default:T):T	=
			peer(it) getOrElse default
			
	def orDefault(default: =>T):Function1[S,T]	=
			orAlways(constant(default))
		
	def orAlways(that:Function[S,T]):Function1[S,T]	=
			it	=> peer(it) getOrElse that(it)
						
	def orElse(that:PFunction[S,T]):PFunction[S,T]	=
			it	=> peer(it) orElse that(it)
		
	/** symbolic alias for andThenFixed */
	def >=>[U](that:PFunction[T,U]):PFunction[S,U]	=
			this andThenFixed that
		
	/** symbolic alias for composeFixed */
	def <=<[R](that:PFunction[R,S]):PFunction[R,T]	=
			this composeFixed that
		
	def andThenFixed[U](that:PFunction[T,U]):PFunction[S,U]	=
			it	=> peer(it) flatMap that
		
	def composeFixed[R](that:PFunction[R,S]):PFunction[R,T]	=
			it	=> that(it) flatMap peer
		
	def toExtractor:Extractor[S,T]	=
			Extractor(peer)
}
