package scutil.pimp

import java.util.concurrent.{ Executor=>JExecutor }

import scala.concurrent.SyncVar

import scutil.lang._

object ExecutorImplicits extends ExecutorImplicits

trait ExecutorImplicits {
	implicit def toExecutorExt(peer:Executor)	= new ExecutorExt(peer)
}

final class ExecutorExt(peer:Executor) {
	/** transports Exceptions (but not every Throwable) to the user of the value */
	def withResult[T](job:Thunk[T]):Thunk[T] = {
		val	out	= new SyncVar[Tried[Exception,T]]
		peer(thunk { out put (Catch.exception get job) })
		thunk { out.get.throwException }
	}
	
	def asJava:JExecutor	= new JExecutor {
		def execute(command:Runnable) {
			peer(command.run)
		}
	}
}
