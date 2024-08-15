package scutil.jcollection.extension

import java.util.{
	List		as JList,
	ArrayList	as JArrayList,
	Collections	as JCollections
}

object SeqJCollectionExtensions {
	extension [T](peer:Seq[T]) {
		def toJList:JList[T]	=  {
			val out	= new JArrayList[T]
			peer foreach out.add
			JCollections.unmodifiableList(out)
		}
	}
}
