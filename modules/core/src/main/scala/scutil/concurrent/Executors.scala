package scutil.concurrent

import java.util.concurrent.{ Executor=>JExecutor }

import scutil.lang._
import scutil.lang.implicits._

object Executors {
	val ignore:Executor	= task	=> ()
	val direct:Executor	= task	=> task()
	val thread:Executor	= task	=> new Thread(task.toRunnable).start
	
	// BETTER move into JExecutorImplicits ?
	def java(executor:JExecutor):Executor	= task => executor execute task.toRunnable
}
