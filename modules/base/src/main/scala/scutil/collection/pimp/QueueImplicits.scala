package scutil.collection.pimp

import scala.collection.immutable.Queue

object QueueImplicits extends QueueImplicits

trait QueueImplicits {
	implicit final class QueueExt[T](peer:Queue[T]) {
		def extractHead:Option[(T,Queue[T])]	=
			if (peer.nonEmpty)	Some(peer.dequeue)
			else				None
	}
}
