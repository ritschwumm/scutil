package scutil.lang.pimp

import scutil.lang._

object Function1Implicits extends Function1Implicits

trait Function1Implicits {
	implicit def toFunction1Ext[S,T](peer:Function1[S,T])	= new Function1Ext[S,T](peer)
}

final class Function1Ext[S,T](peer:Function1[S,T]) {
	/** symbolic alias for andThen */
	def >=>[U](that:Function1[T,U]):Function1[S,U]	=
			peer andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Function1[R,S]):Function1[R,T]	=
			peer compose that
		
	def partial(predicate:Predicate[S]):PartialFunction[S,T]	= 
			new PartialFunction[S,T] {
				def isDefinedAt(s:S):Boolean	= predicate(s)
				def apply(s:S):T				= peer(s)
			}
	
	/** inverse to PartialFunction#lift */
	def unlift[X](implicit ev:PFunction[T,X]):PartialFunction[S,X]	=
			Function unlift peer.asInstanceOf[PFunction[S,X]]
		
	def toPartialFunction:PartialFunction[S,T]	=
			{ case x => peer(x) }
		
	def toPFunction:PFunction[S,T]	=
			it => Some(peer(it))
		
	def toThunk(s:S):Thunk[T]	=
			() => peer(s)
}
