package scutil.lang.pimp

import scala.util.{ Try, Success, Failure }

import scutil.lang._

object TryImplicits extends TryImplicits

trait TryImplicits {
	implicit final class TryExt[T](peer:Try[T]) {
		// 2.12 will have fold, see https://issues.scala-lang.org/browse/SI-8336
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
				Tried fromTry peer
	}
}
