package scutil.pimp

import scutil.lang._

object PFunctionImplicits extends PFunctionImplicits

trait PFunctionImplicits {
	implicit def toPFunctionExt[S,T](peer:PFunction[S,T])	= new PFunctionExt[S,T](peer)
}

final class PFunctionExt[S,T](peer:PFunction[S,T]) {
	def orDefault(value: =>T):Function1[S,T]	=
			orAlways(constant(value))
		
	def orAlways(that:Function[S,T]):Function1[S,T]	=
			it	=> peer(it) getOrElse that(it)
						
	def orElse(that:PFunction[S,T]):PFunction[S,T]	= 
			it	=> peer(it) orElse that(it)
		
	def andThenFixed[U](that:PFunction[T,U]):PFunction[S,U]	=
			it	=> peer(it) flatMap that
		
	def composeThenFixed[R](that:PFunction[R,S]):PFunction[R,T]	=
			it	=> that(it) flatMap peer
		
	/** make PEndo[S] to Endo[S] with getOrElse */
	def toEndo(implicit ev:T=>S):Endo[S]	=
			s => peer(s) map ev getOrElse s
		
	def toExtractor:Extractor[S,T]	=
			Extractor(peer)
}
