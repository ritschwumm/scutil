package scutil.ext

import scala.concurrent.SyncVar

import scutil.Functions._

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
	
	def withResultEither[T](task:Thunk[T]):Thunk[T] = {
		val	out	= new SyncVar[Either[Exception,T]]
		delegate(thunk(out set wrapException(task)))
		thunk(unwrapException(out.get))
	}
	
	private def wrapException[T](task:Thunk[T]):Either[Exception,T]	=
			try { Right(task()) }
			catch { case e:Exception => Left(e) }
			
	private def unwrapException[T](either:Either[Exception,T]):T =
			either match {
				case Right(v)	=> v
				case Left(e)	=> throw e
			}
}
