package scutil.jcollection.extension

import java.util.concurrent.ConcurrentLinkedQueue

object ConcurrentLinkedQueueExtensions {
	implicit final class ConcurrentLinkedQueueExt[T](peer:ConcurrentLinkedQueue[T]) {
		def pollOption():Option[T]	= Option(peer.poll())
		def peekOption:Option[T]	= Option(peer.peek)
	}
}
