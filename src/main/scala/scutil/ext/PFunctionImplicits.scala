package scutil.ext

import scutil.lang._

object PFunctionImplicits extends PFunctionImplicits

trait PFunctionImplicits {
	implicit def toPFunctionExt[S,T](delegate:PFunction[S,T])	= new PFunctionExt[S,T](delegate)
}

final class PFunctionExt[S,T](delegate:PFunction[S,T]) {
	def orDefault(value: =>T):Function1[S,T]	=
			orAlways(constant(value))
		
	def orAlways(that:Function[S,T]):Function1[S,T]	=
			it	=> delegate(it) getOrElse that(it)
						
	def orElse(that:PFunction[S,T]):PFunction[S,T]	= 
			it	=> delegate(it) orElse that(it)
		
	/** make PEndo[S] to Endo[S] with getOrElse */
	def toEndo(implicit ev:T=>S):Endo[S]	=
			s => delegate(s) map ev getOrElse s
		
	def toExtractor:Extractor[S,T]	=
			Extractor(delegate)
}
