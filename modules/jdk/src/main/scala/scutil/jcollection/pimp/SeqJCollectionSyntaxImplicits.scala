package scutil.jcollection.pimp

import java.util.{
	List		=> JList,
	ArrayList	=> JArrayList,
	Collections	=> JCollections
}

object SeqJCollectionSyntaxImplicits extends SeqJCollectionSyntaxImplicits

trait SeqJCollectionSyntaxImplicits {
	implicit final class SeqJCollectionSyntaxExt[T](peer:Seq[T]) {
		def toJList:JList[T]	=  {
			val out	= new JArrayList[T]
			peer foreach out.add
			JCollections unmodifiableList out
		}
	}
}
