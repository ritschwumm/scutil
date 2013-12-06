package scutil.pimp

import scala.util.{ Try, Success, Failure }

import scutil.lang._

object TryImplicits extends TryImplicits

trait TryImplicits {
	implicit def toTryExt[T](peer:Try[T]) = new TryExt[T](peer)
}

final class TryExt[T](peer:Try[T]) {
	def cata[U](failure:Throwable=>U, success:T=>U):U	= 
			peer match {
				case Success(t)	=> success(t)
				case Failure(t)	=> failure(t)
			}
			
	def toEither:Either[Throwable,T]	=
			peer match {
				case Success(t)	=> Right(t)
				case Failure(t)	=> Left(t)	
			}
	
	def toTried:Tried[Throwable,T]	= 
			peer match {
				case Success(t)	=> Win(t)
				case Failure(t)	=> Fail(t)	
			}
}
