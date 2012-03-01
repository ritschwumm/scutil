package scutil.ext

object MapImplicits extends MapImplicits

trait MapImplicits {
	implicit def toMapExt[S,T](delegate:Map[S,T])	= new MapExt(delegate)
}

final class MapExt[S,T](delegate:Map[S,T]) {
	/** map the value for a single key */
	def updatedBy(key:S, func:T=>T):Map[S,T]	=
			delegate get key match {
				case Some(value)	=> delegate + (key -> func(value))
				case None			=> delegate
			}
	
	/** map the value for a single key if it exists or insert a new value for this key */
	def updatedByOrInserted(key:S, update:T=>T, insert: =>T):Map[S,T]	=
			delegate + (key -> (delegate get key map update getOrElse insert))
}
