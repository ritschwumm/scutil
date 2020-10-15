package scutil.jcollection.pimp

import java.util.{
	Map			=> JMap,
	HashMap		=> JHashMap,
	Collections	=> JCollections,
	Properties
}

object MapJCollectionSyntaxImplicits extends MapJCollectionSyntaxImplicits

trait MapJCollectionSyntaxImplicits {
	implicit final class MapJCollectionSyntaxExt[S,T](peer:Map[S,T]) {
		def toJMap:JMap[S,T]	=  {
			val out	= new JHashMap[S,T]
			peer foreach { case (k,v) => out.put(k, v) }
			JCollections unmodifiableMap out
		}

		def toProperties(implicit sev:S=>String, tev:T=>String):Properties	= {
			val out	= new Properties
			peer foreach { case (k, v) => out.setProperty(k, v) }
			out
		}
	}
}
