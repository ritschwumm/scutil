package scutil.worker

import scutil.base.implicits._
import scutil.lang.ISeq

/** a very simple synchronized queue */
final class WorkerQueue[T] {
	private var entries:ISeq[T]	= Vector.empty

	def push(value:T):Unit	=
			synchronized {
				entries	:+= value
			}

	def shift():Option[T]	=
			synchronized {
				entries.extractHead match {
					case Some((head, tail))	=> entries	= tail; Some(head)
					case None				=> None
				}
			}
}
