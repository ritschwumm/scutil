package scutil.pimp

import scala.collection.immutable.Queue

object QueueImplicits extends QueueImplicits

trait QueueImplicits {
	implicit def toQueueExt[T](delegate:Queue[T])	= new QueueExt(delegate)
}

final class QueueExt[T](delegate:Queue[T]) {
	def extractHead:Option[(T,Queue[T])]	=
			if (delegate.nonEmpty)	Some(delegate.dequeue)
			else					None
}
