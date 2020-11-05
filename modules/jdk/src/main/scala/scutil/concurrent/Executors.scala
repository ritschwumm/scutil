package scutil.concurrent

import java.util.concurrent.{ Executor=>JExecutor }

object Executors {
	@deprecated("use Execution", "0.188.0")
	type Executor	= Execution

	@deprecated("use Execution.ignore", "0.188.0")
	val ignore:Executor	= Execution.ignore
	@deprecated("use Execution.direct", "0.188.0")
	val direct:Executor	= Execution.direct
	@deprecated("use Execution.thread", "0.188.0")
	val thread:Executor	= Execution.thread
	@deprecated("use Execution.daemon", "0.188.0")
	val daemon:Executor	= Execution.daemon

	// BETTER move into JExecutorImplicits ?
	@deprecated("use Execution.fromExecutor", "0.188.0")
	def java(executor:JExecutor):Executor	= Execution.fromExecutor(executor)
}
