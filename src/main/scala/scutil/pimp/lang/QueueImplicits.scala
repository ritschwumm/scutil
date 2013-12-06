package scutil.pimp

import scala.collection.immutable.Queue

object QueueImplicits extends QueueImplicits

trait QueueImplicits {
	implicit def toQueueExt[T](peer:Queue[T])	= new QueueExt(peer)
}

final class QueueExt[T](peer:Queue[T]) {
	def extractHead:Option[(T,Queue[T])]	=
			if (peer.nonEmpty)	Some(peer.dequeue)
			else				None
}
