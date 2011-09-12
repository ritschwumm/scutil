package scutil

import java.util.concurrent.Callable

import scutil.Types._

object Functions {
	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()
	def task(value: =>Unit):Task				= thunk(value)
	def thunk[T](value: =>T):Thunk[T]			= () => value
	
	//------------------------------------------------------------------------------
	
	def runnable(task: =>Unit):Runnable 	= runnableTask(thunk(task))
	def runnableTask(task:Task):Runnable	= new Runnable { def run() { task() } }
	
	def callable[T](task: =>T):Callable[T]			= callableTask(thunk(task))
	def callableTask[T](thunk:Thunk[T]):Callable[T]	= new Callable[T] { def call() = thunk() }
}
