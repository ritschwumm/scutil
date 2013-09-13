package scutil.lang

import java.util.concurrent.{ Executor=>JExecutor }
import javax.swing.SwingUtilities

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import scutil.lang._
import scutil.pimp.Function0Implicits._

object Executors {
	import ExecutionContext.Implicits.global
	
	val ignore:Executor	= task	=> ()
	val direct:Executor	= task	=> task()
	val thread:Executor	= task	=> new Thread(task.asRunnable).start
	val spawn:Executor	= task	=> Future { task() }
	val edt:Executor	= task	=> SwingUtilities invokeLater task.asRunnable
	
	// BETTER move into JExecutorImplicits ?
	def java(executor:JExecutor):Executor	= task => executor execute task.asRunnable
}
