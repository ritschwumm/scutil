package scutil

import java.util.concurrent.Callable

object Functions {
	type Task			= Thunk[Unit]
	type Thunk[+T]		= ()=>T		// aka Function0[T] aka Future[T]
	type Effect[-T]		= T=>Unit	// Function1[T,Unit] aka Cont[T]
	type Executor		= Task=>Unit
	
	//def const[S](s:S) = new { def apply[T](t: =>T):S = s }
	def constant[S,T](value: =>T):(S=>T)		= _ => value
	def ignorant[S,T](thunk:Thunk[T]):(S=>T)	= _ => thunk()
	def task(value: =>Unit):Task				= thunk(value)
	def thunk[T](value: =>T):Thunk[T]			= () => value
	
	//------------------------------------------------------------------------------
	
	def runnable(task: =>Unit):Runnable 	= runnableTask(thunk(task))
	def runnableTask(task:Task):Runnable	= new Runnable { def run() { task() } }
	
	def callable[T](task: =>T):Callable[T]			= callableTask(thunk(task))
	def callableTask[T](thunk:Thunk[T]):Callable[T]	= new Callable[T] { def call() = thunk() }
	
	/*
	implicit def functionToRunnable(delegate: =>Unit):Runnable = new Runnable {
		def run() { delegate } 
	}
	
	implicit def functionToCallable[T](delegate: =>T):Callable[T] = new Callable[T] {
		def call():T = delegate
	}
	*/
}
