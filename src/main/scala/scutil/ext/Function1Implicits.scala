package scutil.ext

import scutil.lang._

object Function1Implicits extends Function1Implicits

trait Function1Implicits {
	implicit def toFunction1Ext[S,T](delegate:Function1[S,T])	= new Function1Ext[S,T](delegate)
}

final class Function1Ext[S,T](delegate:Function1[S,T]) {
	def partial(predicate:Predicate[S]):PartialFunction[S,T]	= new PartialFunction[S,T] {
		def isDefinedAt(s:S):Boolean	= predicate(s)
		def apply(s:S):T				= delegate(s)
	}
	
	/** inverse to PartialFunction#lift */
	def unlift[X](implicit ev:PFunction[T,X]):PartialFunction[S,X]	=
			Function unlift delegate.asInstanceOf[PFunction[S,X]]
		
	def toPFunction:PFunction[S,T]	=
			it => Some(delegate(it))
}
