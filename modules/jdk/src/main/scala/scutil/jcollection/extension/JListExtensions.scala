package scutil.jcollection.extension

import java.util.{ List as JList }

object JListExtensions {
	implicit final class JListExt[T](peer:JList[T]) {
		def toVector:Vector[T]		= toIterable.toVector
		def toList:List[T]			= toIterable.toList
		def toSeq:Seq[T]			= toVector
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
