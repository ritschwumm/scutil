package scutil.ext

object Function1Implicits extends Function1Implicits

trait Function1Implicits {
	implicit def toFunction1Ext[S,T](delegate:Function1[S,T]) = new Function1Ext[S,T](delegate)
}

final class Function1Ext[S,T](delegate:Function1[S,T]) {
	def partial(predicate:S=>Boolean):PartialFunction[S,T] = new PartialFunction[S,T] {
		def isDefinedAt(s:S):Boolean	= predicate(s)
		def apply(s:S):T				= delegate(s)
	}
	
	/** inverse to PartialFunction#lift */
	def unlift[X](implicit witness:T=>Option[X]):PartialFunction[S,X] = new PartialFunction[S,X] {
		// TODO @see http://lampsvn.epfl.ch/trac/scala/ticket/3825
		
		// def isDefinedAt(s:S):Boolean	= delegate(s).isDefined
		// def apply(s:S):X	= witness(delegate(s)).get
		
		def isDefinedAt(s:S):Boolean	= lift apply s isDefined;
		def apply(s:S):X				= lift apply s get;
		
		// override def lift	= delegate andThen witness
		override def lift	= delegate.asInstanceOf[S=>Option[X]] 
	}
}
