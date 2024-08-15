package scutil.gui

import javax.swing.SwingUtilities
import javax.swing.Timer

import scutil.core.implicits.*
import scutil.lang.*
import scutil.concurrent.*
import scutil.time.*

object SwingUtil {
	def insideEdt:Boolean	= SwingUtilities.isEventDispatchThread

	val edtExecution:Execution	=
		Execution { task	=>
			SwingUtilities.invokeLater(task.toRunnable)
		}

	//------------------------------------------------------------------------------
	//## unsafe

	// transports Exceptions but not every Throwable
	def worker[T](job: =>T):Thunk[T]	= Execution.thread	.withResult(thunk(job))

	// transports Exceptions but not every Throwable
	def edt[T](job: =>T):Thunk[T]		= edtExecution		.withResult(thunk(job))

	/** classical SwingWorker pattern without exception handling */
	def swingWorker[T](calculate:Thunk[T], use:Effect[T]):Unit	= {
		worker {
			val	value	= calculate()
			edt {
				use(value)
			}
		}
	}

	//------------------------------------------------------------------------------
	//## safe

	def edtIoResource[T](peer:IoResource[T]):IoResource[T]	=
		edtExecution.wrapIoResource(peer)

	def edtSubmitIo(peer:Io[Unit]):Io[Unit]	=
		edtExecution.submitIo(peer)

	def edtIo[T](peer:Io[T]):Io[Io[T]]	=
		edtExecution.withResultIo(peer)

	def workerIo[T](peer:Io[T]):Io[Io[T]]	=
		Execution.thread.withResultIo(peer)

	def swingTimerIoResource(updateDelay:MilliDuration, action:Io[Unit]):IoResource[Unit]	=
		IoResource.unsafe.disposing {
			new Timer(
				updateDelay.millis.toInt,
				_ => action.unsafeRun()
			)
			.doto(_.start())
		}{
			_.stop()
		}
		.void

	/** safe SwingWorker pattern */
	def swingWorkerIo[T](calculate:Io[T], use:T=>Io[Unit]):Io[Unit]	=
		// TODO thread test this
		workerIo(
			calculate.flatMap(value =>
				edtSubmitIo(use(value))
			)
		).void
}
