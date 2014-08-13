package scutil.collection.pimp

import scutil.lang._

object MapImplicits extends MapImplicits

trait MapImplicits {
	implicit def toMapExt[S,T](peer:Map[S,T])	= new MapExt(peer)
}

final class MapExt[S,T](peer:Map[S,T]) {
	// aliases with low precendence
	
	def put(key:S, value:T):Map[S,T]	= peer + (key -> value)
	def remove(key:S):Map[S,T]			= peer - key
	
	/** remove an element and return it */
	def extractAt(s:S):Option[(T,Map[S,T])]	=
			peer get s map { t => (t, peer - s) }
		
	/** inverse filterKeys */
	def filterNotKeys(pred:Predicate[S]):Map[S,T]	=
			peer filterKeys { it => !pred(it) } 
		
	def partitionKeys(pred:Predicate[S]):(Map[S,T],Map[S,T])	=
			peer partition { case (k, _)	=> pred(k) }
		
	/** set or remove the value */
	def set(key:S, value:Option[T]):Map[S,T]	=
			value match {
				case Some(value)	=> peer + (key -> value)
				case None			=> peer - key
			}
			
	/** set or remove multiple values */
	def setMany(it:Traversable[(S,Option[T])]):Map[S,T]	=
			(it foldLeft peer) { (orig, change) =>
				change match {
					case (k, Some(v))	=> orig + (k -> v)
					case (k, None)		=> orig - k
				}
			}
			
	def setManyBy(func:(S,T)=>Option[T]):Map[S,T]	=
			peer flatMap { case (k,v) =>
				func(k, v) map { k -> _ }
			}
			
	/** set or remove the value for a single key */
	def setBy(key:S, func:PEndo[T]):Map[S,T]	=
			peer get key flatMap func match {
				case Some(value)	=> peer + (key -> value)
				case None			=> peer
			}
			
	/** map the value for a single key */
	def updatedBy(key:S, func:Endo[T]):Map[S,T]	=
			peer get key match {
				case Some(value)	=> peer + (key -> func(value))
				case None			=> peer
			}
			
	def updatedManyBy(func:(S,T)=>T):Map[S,T]	=
			peer map { case (k,v) =>
				k -> func(k, v)
			}
			
	/** map the value for a single key if it exists or insert a new value for this key */
	def updatedByOrInserted(key:S, update:Endo[T], insert: =>T):Map[S,T]	=
			peer + (key -> (peer get key map update getOrElse insert))
}
