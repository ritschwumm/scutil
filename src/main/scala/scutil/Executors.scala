package scutil

import javax.swing.SwingUtilities

import scutil.Types._
import scutil.Functions._

object Executors {
	// TODO add synchronized
	// TODO add ThreadPool
	
	val ignore:Executor	= task	=> ()
	val direct:Executor	= task	=> task()
	val thread:Executor	= task	=> new Thread(runnableTask(task)).start
	val spawn:Executor	= task	=> scala.concurrent.ops.spawn { task() }
	val edt:Executor	= task	=> SwingUtilities invokeLater runnableTask(task)
}
