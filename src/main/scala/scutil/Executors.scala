package scutil

import java.util.concurrent.{ Executor=>JExecutor }
import javax.swing.SwingUtilities

import scutil.lang._
import scutil.ext.Function0Implicits._

object Executors {
	val ignore:Executor	= task	=> ()
	val direct:Executor	= task	=> task()
	val thread:Executor	= task	=> new Thread(task.asRunnable).start
	val spawn:Executor	= task	=> scala.concurrent.ops.spawn { task() }
	val edt:Executor	= task	=> SwingUtilities invokeLater task.asRunnable
	
	// BETTER move into JExecutorImplicits ?
	def java(executor:JExecutor):Executor	= task => executor execute task.asRunnable
}
