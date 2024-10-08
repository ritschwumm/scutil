package scutil.jcollection.extension

import java.util.{
	Set			as JSet,
	HashSet		as JHashSet,
	Collections	as JCollections
}

object SetJCollectionExtensions {
	extension [T](peer:Set[T]) {
		def toJSet:JSet[T]	=  {
			val out	= new JHashSet[T]
			peer foreach out.add
			JCollections.unmodifiableSet(out)
		}
	}
}
