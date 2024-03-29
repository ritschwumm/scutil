package scutil.jcollection.extension

import java.util.{
	List		as JList,
	ArrayList	as JArrayList,
	Collections	as JCollections
}

object SeqJCollectionExtensions {
	implicit final class SeqJCollectionSyntaxExt[T](peer:Seq[T]) {
		def toJList:JList[T]	=  {
			val out	= new JArrayList[T]
			peer foreach out.add
			JCollections unmodifiableList out
		}
	}
}
