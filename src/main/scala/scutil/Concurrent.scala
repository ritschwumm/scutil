package scutil

import java.util.concurrent.Callable

import scala.concurrent.SyncVar

import scutil.Functions._

object Concurrent {
	def execute[T](execute:Executor, task:Thunk[T]):Thunk[T] = {
		val	out	= new SyncVar[T]
		execute(thunk(out set task()))
		thunk(out.get)
	}
	
	def executeWrap[T](execute:Executor, task:Thunk[T]):Thunk[T] = {
		val	out	= new SyncVar[Either[Exception,T]]
		execute(thunk(out set wrapException(task)))
		thunk(unwrapException(out.get))
	}
	
	def wrapException[T](task:Thunk[T]):Either[Exception,T]	=
			try { Right(task()) }
			catch { case e:Exception => Left(e) }
			
	def unwrapException[T](either:Either[Exception,T]):T =
			either match {
				case Right(v)	=> v
				case Left(e)	=> throw e
			}
			
	//------------------------------------------------------------------------------
	
	val ignoreExecutor:Executor		= (task:Task) => ()
	val directExecutor:Executor		= (task:Task) => task()
	val backgroundExecutor:Executor	= (task:Task) => new Thread(runnableTask(task)).start
	val spawnExecutor:Executor		= (task:Task) => scala.concurrent.ops.spawn { task() }
	
	//------------------------------------------------------------------------------
	
	def installDefaultUncaughtExceptionHandler(handler:(Thread,Throwable)=>Unit) {
		Thread setDefaultUncaughtExceptionHandler threadExceptionHandler(handler)
	}
	
	def installUncaughtExceptionHandler(thread:Thread)(handler:(Thread,Throwable)=>Unit) {
		thread setUncaughtExceptionHandler threadExceptionHandler(handler)
	}
	
	def threadExceptionHandler(handler:(Thread,Throwable)=>Unit) =
			new Thread.UncaughtExceptionHandler {
				def uncaughtException(t:Thread, e:Throwable) { 
					handler(t, e) 
				}
			}
}
