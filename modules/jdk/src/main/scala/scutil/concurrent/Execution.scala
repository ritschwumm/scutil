package scutil.concurrent

import java.util.concurrent.*

import scutil.lang.*
import scutil.lang.implicits.*

object Execution {
	val ignore:Execution	= Execution { task	=> ()		}
	val direct:Execution	= Execution { task	=> task()	}
	val thread:Execution	= Execution { task	=> new Thread(task.toRunnable).start()	}
	val daemon:Execution	= Execution { task	=> new Thread(task.toRunnable).doto( _ setDaemon true).start() }

	// BETTER move into some JExecutorExtensions ?
	def fromExecutor(executor:Executor):Execution	= Execution { task => executor execute task.toRunnable }
}

// NOTE this is equivalent to Later[Unit]
// TODO the ()=>Unit in here should be an Io, right?
final case class Execution(submit:(()=>Unit)=>Unit) {
	/** transports Exceptions (but not every Throwable) to the user of the value */
	def withResult[T](job:Thunk[T]):Thunk[T] = {
		val	out	= new LinkedBlockingQueue[Either[Exception,T]](1)
		submit(thunk { out put (Catch.exception get job) })
		thunk { out.take().throwException }
	}

	def submitIo(action:Io[Unit]):Io[Unit]	=
		Io delay {
			submit(action.unsafeRun)
		}

	def withResultIo[T](action:Io[T]):Io[Io[T]]	=
		Io delay {
			val running	= withResult(action.unsafeRun)
			Io delay { running() }
		}


	def wrapIoResource[T](resource:IoResource[T]):IoResource[T]	=
		IoResource(
			Io delay {
				val (value, disposer)	= withResult{ () => resource.open.unsafeRun() }()
				val disposer2			= Io delay { withResult{ () => disposer.unsafeRun() }() }
				value -> disposer2
			}
		)

	// TODO thread test this
	def wrapIoResource2[T](resource:IoResource[T]):IoResource[T]	=
		IoResource(
			withResultIo(resource.open).flatten.map((value, disposer) =>
				value	-> withResultIo(disposer).flatten
			)
		)

	def toExecutor:Executor	=
		(command:Runnable) => submit(command.run _)
}
