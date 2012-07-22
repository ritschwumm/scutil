package scutil.gui

import javax.swing.SwingUtilities

import scutil.lang._
import scutil.Executors
import scutil.ext.ExecutorImplicits._

object SwingUtil {
	def withinEDT:Boolean	= SwingUtilities.isEventDispatchThread
	
	def worker[T](job: =>T):Thunk[T]	= Executors.thread withResultEither thunk(job)
	def workerWait[T](job: => T):T		= worker(job)()
	
	def edt[T](job: =>T):Thunk[T]		= Executors.edt withResultEither thunk(job)
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
}
