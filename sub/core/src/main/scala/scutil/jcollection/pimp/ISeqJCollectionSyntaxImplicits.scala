package scutil.jcollection.pimp

import java.util.{
	List		=> JList,
	ArrayList	=> JArrayList,
	Collections	=> JCollections
}

import scutil.lang.ISeq

object ISeqJCollectionSyntaxImplicits extends ISeqJCollectionSyntaxImplicits

trait ISeqJCollectionSyntaxImplicits {
	implicit def toISeqJCollectionSyntaxExt[T](peer:ISeq[T]) = new ISeqJCollectionSyntaxExt(peer)
}

final class ISeqJCollectionSyntaxExt[T](peer:ISeq[T]) {
	def toJList:JList[T]	=  {
		val out	= new JArrayList[T]
		peer foreach out.add
		JCollections unmodifiableList out
	}
}
