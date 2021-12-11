package scutil.jcollection.extension

import java.util.Properties

import scala.collection.immutable.HashMap

object PropertiesExtensions {
	implicit final class PropertiesExt(peer:Properties) {
		def toHashMap:Map[String,String]	= {
			var out		= HashMap.empty[String,String]
			val keyIter	= peer.stringPropertyNames.iterator
			while (keyIter.hasNext) {
				val key		= keyIter.next
				val value	= peer getProperty key
				out	+= (key -> value)
			}
			out
		}

		def toMap:Map[String,String]	= toHashMap
	}
}
