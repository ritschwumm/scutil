package scutil.concurrent

import java.util.concurrent.{ Executor=>JExecutor }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import scutil.lang._
import scutil.lang.implicits._

object Executors {
	import ExecutionContext.Implicits.global
	
	val ignore:Executor	= task	=> ()
	val direct:Executor	= task	=> task()
	val thread:Executor	= task	=> new Thread(task.asRunnable).start
	val spawn:Executor	= task	=> Future { task() }
	
	// BETTER move into JExecutorImplicits ?
	def java(executor:JExecutor):Executor	= task => executor execute task.asRunnable
}
