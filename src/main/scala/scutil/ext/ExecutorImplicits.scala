package scutil.ext

import scala.concurrent.SyncVar

import scutil.lang._
import scutil.tried._

object ExecutorImplicits extends ExecutorImplicits

trait ExecutorImplicits {
	implicit def toExecutorExt(delegate:Executor)	= new ExecutorExt(delegate)
}

final class ExecutorExt(delegate:Executor) {
	def withResult[T](task:Thunk[T]):Thunk[T] = {
		val	out	= new SyncVar[T]
		delegate(thunk(out set task()))
		thunk(out.get)
	}
	
	def withResultTried[T](task:Thunk[T]):Thunk[T] = {
		val	out	= new SyncVar[Tried[Exception,T]]
		delegate(thunk(out set wrapException(task)))
		thunk(unwrapException(out.get))
	}
	
	private def wrapException[T](task:Thunk[T]):Tried[Exception,T]	=
			try { Win(task()) }
			catch { case e:Exception => Fail(e) }
			
	private def unwrapException[T](tried:Tried[Exception,T]):T =
			tried match {
				case Win(v)		=> v
				case Fail(e)	=> throw e
			}
}
