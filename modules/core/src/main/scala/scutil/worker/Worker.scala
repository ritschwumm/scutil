package scutil.worker

import scala.annotation.tailrec

import scutil.lang._
import scutil.base.implicits._
import scutil.time._

final class Worker(name:String, delay:MilliDuration, task:Thunk[Unit], error:Effect[Exception] = _ => ()) extends Disposable {
	private val shortly	= 50.millis

	@volatile
	private var state:WorkerState					= WorkerState.Waiting
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
		queue push WorkerCommand.Start
	}

	/** stop working, can be restarted */
	def stop():Unit = {
		queue push WorkerCommand.Stop
	}

	/** stop working, release resources asap, then die */
	def dispose():Unit = {
		queue push WorkerCommand.Die
	}

	/** to be called after stop or dispose to make sure the task is not executed any more */
	def awaitWorkless():Unit = {
		while (state == WorkerState.Working) {
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
			case (WorkerState.Waiting,		None)						=> WorkerState.Waiting
			case (WorkerState.Waiting,		Some(WorkerCommand.Start))	=> WorkerState.Working
			case (WorkerState.Waiting,		Some(WorkerCommand.Stop))	=> WorkerState.Waiting
			case (WorkerState.Waiting,		Some(WorkerCommand.Die))	=> WorkerState.Dead
			case (WorkerState.Working,		None)						=> WorkerState.Sleeping(delay - shortly)
			case (WorkerState.Working,		Some(WorkerCommand.Start))	=> WorkerState.Working
			case (WorkerState.Working,		Some(WorkerCommand.Stop))	=> WorkerState.Waiting
			case (WorkerState.Working,		Some(WorkerCommand.Die))	=> WorkerState.Dead
			case (WorkerState.Sleeping(x),	None)						=>
					if (x > MilliDuration.zero)		WorkerState.Sleeping(x - shortly)
					else							WorkerState.Working
			case (WorkerState.Sleeping(_),	Some(WorkerCommand.Start))	=> WorkerState.Working
			case (WorkerState.Sleeping(_),	Some(WorkerCommand.Stop))	=> WorkerState.Waiting
			case (WorkerState.Sleeping(_),	Some(WorkerCommand.Die))	=> WorkerState.Dead
			case (WorkerState.Dead,			_)					=> WorkerState.Dead
		}
		state match {
			case WorkerState.Working		=> work();			loop()
			case WorkerState.Sleeping(_)	=> nap(shortly);	loop()
			case WorkerState.Waiting		=> nap(shortly);	loop()
			case WorkerState.Dead			=>
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
