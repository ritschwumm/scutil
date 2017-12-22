package scutil.jcollection.pimp

import java.util.{ List => JList }

import scutil.lang.ISeq

object JListImplicits extends JListImplicits

trait JListImplicits {
	implicit final class JListExt[T](peer:JList[T]) {
		def toVector:ISeq[T]		= toIterable.toVector
		def toList:List[T]			= toIterable.toList
		def toISeq:ISeq[T]			= toVector
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
