package scutil.worker

import scala.annotation.tailrec

import scutil.lang._
import scutil.base.implicits._
import scutil.time._

final class Worker(name:String, delay:MilliDuration, task:Thunk[Unit], error:Effect[Exception] = _ => ()) extends Disposable {
	private val shortly	= 50.millis

	@volatile
	private var state:WorkerState					= WorkerWaiting
	private val queue:WorkerQueue[WorkerCommand]	= new WorkerQueue[WorkerCommand]

	private object thread extends Thread {
		setName(name)
		setPriority(Thread.MIN_PRIORITY)
		override def run():Unit = { loop() }
		start()
	}

	//------------------------------------------------------------------------------

	/** start working */
	def start():Unit = {
		// access thread to instantiate it
		thread
		queue push WorkerStart
	}

	/** stop working, can be restarted */
	def stop():Unit = {
		queue push WorkerStop
	}

	/** stop working, release resources asap, then die */
	def dispose():Unit = {
		queue push WorkerDie
	}

	/** to be called after stop or dispose to make sure the task is not executed any more */
	def awaitWorkless():Unit = {
		while (state == WorkerWorking) {
			nap(shortly)
		}
	}

	def join():Unit = {
		thread.join()
	}

	def disposeAndWait():Unit = {
		dispose()
		awaitWorkless()
		join()
	}

	//------------------------------------------------------------------------------

	@tailrec
	private def loop():Unit = {
		val command	= queue.shift()
		state	= (state, command) match {
			case (WorkerWaiting,		None)				=> WorkerWaiting
			case (WorkerWaiting,		Some(WorkerStart))	=> WorkerWorking
			case (WorkerWaiting,		Some(WorkerStop))	=> WorkerWaiting
			case (WorkerWaiting,		Some(WorkerDie))	=> WorkerDead
			case (WorkerWorking,		None)				=> WorkerSleeping(delay - shortly)
			case (WorkerWorking,		Some(WorkerStart))	=> WorkerWorking
			case (WorkerWorking,		Some(WorkerStop))	=> WorkerWaiting
			case (WorkerWorking,		Some(WorkerDie))	=> WorkerDead
			case (WorkerSleeping(x),	None)				=>
					if (x > MilliDuration.zero)		WorkerSleeping(x - shortly)
					else							WorkerWorking
			case (WorkerSleeping(_),	Some(WorkerStart))	=> WorkerWorking
			case (WorkerSleeping(_),	Some(WorkerStop))	=> WorkerWaiting
			case (WorkerSleeping(_),	Some(WorkerDie))	=> WorkerDead
			case (WorkerDead,			_)					=> WorkerDead
		}
		state match {
			case WorkerWorking		=> work();			loop()
			case WorkerSleeping(_)	=> nap(shortly);	loop()
			case WorkerWaiting		=> nap(shortly);	loop()
			case WorkerDead			=>
		}
	}

	private def work():Unit = {
		try {
			task()
		}
		catch { case e:Exception =>
			error(e)
		}
	}

	private def nap(duration:MilliDuration):Unit = {
		Thread sleep duration.millis
	}
}
