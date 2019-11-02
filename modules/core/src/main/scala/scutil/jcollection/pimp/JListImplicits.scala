package scutil.jcollection.pimp

import java.util.{ List => JList }

import scutil.lang.ISeq

object JListImplicits extends JListImplicits

trait JListImplicits {
	implicit final class JListExt[T](peer:JList[T]) {
		def toVector:Vector[T]		= toIterable.toVector
		def toList:List[T]			= toIterable.toList
		@deprecated("use toSeq", "0.162.0")
		def toISeq:ISeq[T]			= toSeq
		def toSeq:Seq[T]			= toVector
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
