package scutil.gui

import scutil.Types._
import scutil.Functions._
import scutil.Concurrent._
import scutil.Executors

object SwingUtil {
	def worker[T](job: =>T):Thunk[T]	= executeWrap(Executors.thread,	thunk(job))
	def workerWait[T](job: => T):T		= worker(job)()
	
	def edt[T](job: =>T):Thunk[T]		= executeWrap(Executors.edt,	thunk(job))
	def edtWait[T](job: =>T):T			= edt(job)()
	
	/** classical SwingWorker pattern without exception handling */
	def work[T](calculate:Thunk[T], use:Effect[T]) {
		worker {
			val	value	= calculate()
			edt {
				use(value)
			}
		}
	}
}
