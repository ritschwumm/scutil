package scutil.concurrent

import java.util.concurrent._

import scutil.lang._
import scutil.lang.implicits._

object Execution {
	val ignore:Execution	= Execution { task	=> ()		}
	val direct:Execution	= Execution { task	=> task()	}
	val thread:Execution	= Execution { task	=> new Thread(task.toRunnable).start()	}
	val daemon:Execution	= Execution { task	=> new Thread(task.toRunnable).doto( _ setDaemon true).start() }

	// BETTER move into JExecutorImplicits ?
	def fromExecutor(executor:Executor):Execution	= Execution { task => executor execute task.toRunnable }
}

// NOTE this is equivalent to Later[Unit]
final case class Execution(submit:(()=>Unit)=>Unit) {
	/** transports Exceptions (but not every Throwable) to the user of the value */
	def withResult[T](job:Thunk[T]):Thunk[T] = {
		val	out	= new LinkedBlockingQueue[Either[Exception,T]](1)
		submit(thunk { out put (Catch.exception get job) })
		thunk { out.take().throwException }
	}

	def wrapUsing[T](using:Using[T]):Using[T]	=
		() => {
			val (resource, disposer)	= withResult{ () => using.open() }()
			val disposer2	= Disposer delay { withResult{ () => disposer.dispose() }() }
			resource -> disposer2
		}

	def toExecutor:Executor	=
		(command:Runnable) => submit(command.run _)
}
