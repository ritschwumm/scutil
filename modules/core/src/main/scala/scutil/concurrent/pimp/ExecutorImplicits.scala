package scutil.concurrent.pimp

import java.util.concurrent.{ Executor=>JExecutor, LinkedBlockingQueue }

import scutil.lang._
import scutil.lang.pimp.EitherImplicits._

object ExecutorImplicits extends ExecutorImplicits

trait ExecutorImplicits {
	implicit final class ExecutorExt(peer:Executor) {
		/** transports Exceptions (but not every Throwable) to the user of the value */
		def withResult[T](job:Thunk[T]):Thunk[T] = {
			val	out	= new LinkedBlockingQueue[Either[Exception,T]](1)
			peer(thunk { out put (Catch.exception get job) })
			thunk { out.take().throwException }
		}

		def toJavaExecutor:JExecutor	=
			new JExecutor {
				def execute(command:Runnable):Unit = {
					peer(command.run _)
				}
			}
	}
}
