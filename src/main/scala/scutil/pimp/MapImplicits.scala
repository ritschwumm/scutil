package scutil.pimp

import scutil.lang._

object MapImplicits extends MapImplicits

trait MapImplicits {
	implicit def toMapExt[S,T](peer:Map[S,T])	= new MapExt(peer)
}

final class MapExt[S,T](peer:Map[S,T]) {
	/** remove an element and return it */
	def extractAt(s:S):Option[(T,Map[S,T])]	=
			peer get s map { t => (t, peer - s) }
		
	/** inverse filterKeys */
	def filterNotKeys(pred:Predicate[S]):Map[S,T]	=
			peer filterKeys { it => !pred(it) } 
		
	/** set or remove the value */
	def set(key:S, value:Option[T]):Map[S,T]	=
			value match {
				case Some(value)	=> peer + (key -> value)
				case None			=> peer - key
			}
			
	/** map the value for a single key */
	def updatedBy(key:S, func:Endo[T]):Map[S,T]	=
			peer get key match {
				case Some(value)	=> peer + (key -> func(value))
				case None			=> peer
			}
	
	/** map the value for a single key if it exists or insert a new value for this key */
	def updatedByOrInserted(key:S, update:Endo[T], insert: =>T):Map[S,T]	=
			peer + (key -> (peer get key map update getOrElse insert))
}
