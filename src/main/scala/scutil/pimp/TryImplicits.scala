package scutil.pimp

import scala.util.{ Try, Success, Failure }

import scutil.lang._

object TryImplicits extends TryImplicits

trait TryImplicits {
	implicit def toTryExt[T](delegate:Try[T]) = new TryExt[T](delegate)
}

final class TryExt[T](delegate:Try[T]) {
	def cata[U](failure:Throwable=>U, success:T=>U):U	= 
			delegate match {
				case Success(t)	=> success(t)
				case Failure(t)	=> failure(t)
			}
			
	def toEither:Either[Throwable,T]	=
			delegate match {
				case Success(t)	=> Right(t)
				case Failure(t)	=> Left(t)	
			}
	
	def toTried:Tried[Throwable,T]	= 
			delegate match {
				case Success(t)	=> Win(t)
				case Failure(t)	=> Fail(t)	
			}
}
