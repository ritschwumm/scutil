package scutil.jcollection.extension

import java.util.{
	Set			=> JSet,
	HashSet		=> JHashSet,
	Collections	=> JCollections
}

object SetJCollectionSyntaxImplicits {
	implicit final class SetJCollectionSyntaxExt[T](peer:Set[T]) {
		def toJSet:JSet[T]	=  {
			val out	= new JHashSet[T]
			peer foreach out.add
			JCollections unmodifiableSet out
		}
	}
}
