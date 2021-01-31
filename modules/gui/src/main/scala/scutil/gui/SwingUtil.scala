package scutil.gui

import javax.swing.SwingUtilities
import javax.swing.Timer

import scutil.core.implicits._
import scutil.lang._
import scutil.concurrent._
import scutil.time._

object SwingUtil {
	def insideEdt:Boolean		= SwingUtilities.isEventDispatchThread

	val edtExecution:Execution	=
		Execution { task	=>
			SwingUtilities invokeLater task.toRunnable
		}

	def edtUsing[T](peer:Using[T]):Using[T]	=
		edtExecution wrapUsing peer

	// transports Exceptions but not every Throwable
	def worker[T](job: =>T):Thunk[T]	= Execution.thread	withResult thunk(job)

	// transports Exceptions but not every Throwable
	def edt[T](job: =>T):Thunk[T]		= edtExecution		withResult thunk(job)

	def swingTimerUsing(updateDelay:MilliDuration, action:Io[Unit]):Using[Unit]	=
		Using.of{ () =>
			new Timer(
				updateDelay.millis.toInt,
				_ => action.unsafeRun()
			)
			.doto(_.start())
		}{
			_.stop()
		}
		.void

	/** classical SwingWorker pattern without exception handling */
	def swingWorker[T](calculate:Thunk[T], use:Effect[T]):Unit	= {
		worker {
			val	value	= calculate()
			edt {
				use(value)
			}
		}
	}
}
