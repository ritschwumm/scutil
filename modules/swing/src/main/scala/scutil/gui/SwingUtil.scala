package scutil.gui

import javax.swing.SwingUtilities

import scutil.lang._
import scutil.lang.implicits._
import scutil.concurrent.Executors
import scutil.concurrent.implicits._

object SwingUtil {
	def insideEDT:Boolean		= SwingUtilities.isEventDispatchThread
	val edtExecutor:Executor	= task	=> SwingUtilities invokeLater task.toRunnable
	
	// transports Exceptions but not every Throwable
	def worker[T](job: =>T):Thunk[T]	= Executors.thread	withResult thunk(job)
	def workerWait[T](job: => T):T		= worker(job)()
	
	// transports Exceptions but not every Throwable
	def edt[T](job: =>T):Thunk[T]		= edtExecutor		withResult thunk(job)
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