package scutil.lang.pimp

import scala.util.{ Try, Success, Failure }

import scutil.base.implicits._
import scutil.lang._
import scutil.lang.tc._

object TryImplicits extends TryImplicits

trait TryImplicits {
	implicit final class TryExt[T](peer:Try[T]) {
		// 2.12 will have fold, see https://issues.scala-lang.org/browse/SI-8336
		def cata[U](failure:Throwable=>U, success:T=>U):U	=
				peer match {
					case Failure(t)	=> failure(t)
					case Success(t)	=> success(t)
				}
				
		def toEither:Either[Throwable,T]	=
				peer match {
					case Failure(t)	=> Left(t)
					case Success(t)	=> Right(t)
				}
				
		def toValidated:Validated[Throwable,T]	=
				peer match {
					case Failure(t)	=> Bad(t)
					case Success(t)	=> Good(t)
				}
				
		def toEitherT[F[_]:Applicative]:EitherT[F,Throwable,T]	=
				toEither.toEitherT
	}
}
