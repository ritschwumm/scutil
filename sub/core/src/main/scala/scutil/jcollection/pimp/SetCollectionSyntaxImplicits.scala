package scutil.jcollection.pimp

import java.util.{
	Set			=> JSet,
	HashSet		=> JHashSet,
	Collections	=> JCollections
}

object SetJCollectionSyntaxImplicits extends SetJCollectionSyntaxImplicits

trait SetJCollectionSyntaxImplicits {
	implicit def toSetJCollectionSyntaxExt[T](peer:Set[T])	= new SetJCollectionSyntaxExt(peer)
}

final class SetJCollectionSyntaxExt[T](peer:Set[T]) {
	def toJSet:JSet[T]	=  {
		val out	= new JHashSet[T]
		peer foreach out.add
		JCollections unmodifiableSet out
	}
}
