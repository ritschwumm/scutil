package scutil.concurrent

import java.util.concurrent.atomic.AtomicBoolean

import scutil.core.implicits._
import scutil.lang._

object SimpleWorker {
	@deprecated("use SimpleWorker.ioResource", "0.203.0")
	def build(name:String, priority:Int, action:Io[Boolean]):Using[Unit]	=
		Using.of { () =>
			// used in addition to checking the interrupted status to prevent
			// hanging Threads when a malicious actor like SourceDataLine.write
			// clears the interrupted flag without throwing an InterruptedException
			val keepOn	= new AtomicBoolean(true)
			val thread	=
				new Thread {
					override def run():Unit =
						try {
							while (keepOn.get() && !isInterrupted() && action.unsafeRun()) {}
						}
						catch { case e:InterruptedException =>
							// just exit
						}
				}
			thread.setName(name)
			thread.setPriority(priority)
			thread.start()
			thread -> keepOn
		}{ case (thread, keepOn) =>
			keepOn.set(false)
			thread.interrupt()
			thread.join()
		}
		.void

	def ioResource(name:String, priority:Int, action:Io[Boolean]):IoResource[Unit]	=
		IoResource.unsafe.disposing {
			// used in addition to checking the interrupted status to prevent
			// hanging Threads when a malicious actor like SourceDataLine.write
			// clears the interrupted flag without throwing an InterruptedException
			val keepOn	= new AtomicBoolean(true)
			val thread	=
				new Thread {
					override def run():Unit =
						try {
							while (keepOn.get() && !isInterrupted() && action.unsafeRun()) {}
						}
						catch { case e:InterruptedException =>
							// just exit
						}
				}
			thread.setName(name)
			thread.setPriority(priority)
			thread.start()
			thread -> keepOn
		}{ case (thread, keepOn) =>
			keepOn.set(false)
			thread.interrupt()
			thread.join()
		}
		.void
}
