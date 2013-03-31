package scutil.gui

import javax.swing.SwingUtilities

import scutil.lang._
import scutil.Executors
import scutil.tried._
import scutil.ext.ExecutorImplicits._

object SwingUtil {
	def insideEDT:Boolean	= SwingUtilities.isEventDispatchThread
	
	// transports Exceptions but not every Throwable
	def worker[T](job: =>T):Thunk[T]	= Executors.thread	withResult thunk(job)
	def workerWait[T](job: => T):T		= worker(job)()
	
	// transports Exceptions but not every Throwable
	def edt[T](job: =>T):Thunk[T]		= Executors.edt		withResult thunk(job)
	def edtWait[T](job: =>T):T			= edt(job)()
	
	/** classical SwingWorker pattern without exception handling */
	def swingWorker[T](calculate:Thunk[T], use:Effect[T]) {
		worker {
			val	value	= calculate()
			edt {
				use(value)
			}
		}
	}
	
	/** classical SwingWorker pattern, but with exception handling */
	def swingWorkerException[T](calculate:Thunk[T], use:Effect[Tried[Exception,T]]) {
		worker {
			val	value	= Tried exceptionCatch calculate()
			edt {
				use(value)
			}
		}
	}
}
