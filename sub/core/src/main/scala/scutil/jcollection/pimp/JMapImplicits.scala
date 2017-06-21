package scutil.jcollection.pimp

import java.util.{ Map => JMap }

import scala.collection.immutable.HashMap

object JMapImplicits extends JMapImplicits

trait JMapImplicits {
	implicit final class JMapExt[K,V](peer:JMap[K,V]) {
		def toHashMap:Map[K,V]	= {
			var out		= HashMap.empty[K,V]
			val keyIter	= peer.keySet.iterator
			while (keyIter.hasNext) {
				val key		= keyIter.next
				val value	= peer get key
				out	+= (key -> value)
			}
			out
		}
		
		def toMap:Map[K,V]	= toHashMap
	}
}
