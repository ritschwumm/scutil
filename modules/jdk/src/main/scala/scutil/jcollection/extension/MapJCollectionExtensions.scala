package scutil.jcollection.extension

import java.util.{
	Map			as JMap,
	HashMap		as JHashMap,
	Collections	as JCollections,
	Properties
}

object MapJCollectionExtensions {
	extension [S,T](peer:Map[S,T]) {
		def toJMap:JMap[S,T]	=  {
			val out	= new JHashMap[S,T]
			peer foreach { case (k,v) => out.put(k, v) }
			JCollections.unmodifiableMap(out)
		}

		def toProperties(using sev:S <:< String, tev: T <:< String):Properties	= {
			val out	= new Properties
			peer foreach { case (k, v) => out.setProperty(k, v) }
			out
		}
	}
}
