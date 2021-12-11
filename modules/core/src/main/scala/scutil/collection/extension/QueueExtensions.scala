package scutil.collection.extension

import scala.collection.immutable.Queue

object QueueExtensions {
	implicit final class QueueExt[T](peer:Queue[T]) {
		def extractHead:Option[(T,Queue[T])]	=
			if (peer.nonEmpty)	Some(peer.dequeue)
			else				None
	}
}
