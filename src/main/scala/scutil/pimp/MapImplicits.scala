package scutil.pimp

import scutil.lang._

object MapImplicits extends MapImplicits

trait MapImplicits {
	implicit def toMapExt[S,T](delegate:Map[S,T])	= new MapExt(delegate)
}

final class MapExt[S,T](delegate:Map[S,T]) {
	/** remove an element and return it */
	def extractAt(s:S):Option[(T,Map[S,T])]	=
			delegate get s map { t => (t, delegate - s) }
		
	/** inverse filterKeys */
	def filterNotKeys(pred:Predicate[S]):Map[S,T]	=
			delegate filterKeys { it => !pred(it) } 
		
	/** set or remove the value */
	def set(key:S, value:Option[T]):Map[S,T]	=
			value match {
				case Some(value)	=> delegate + (key -> value)
				case None			=> delegate - key
			}
			
	/** map the value for a single key */
	def updatedBy(key:S, func:Endo[T]):Map[S,T]	=
			delegate get key match {
				case Some(value)	=> delegate + (key -> func(value))
				case None			=> delegate
			}
	
	/** map the value for a single key if it exists or insert a new value for this key */
	def updatedByOrInserted(key:S, update:Endo[T], insert: =>T):Map[S,T]	=
			delegate + (key -> (delegate get key map update getOrElse insert))
}
