package scutil.lang.extension

import scala.util.{ Try, Success, Failure }

import scutil.core.implicits._
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

		def toValidated:Validated[Throwable,T]	=
			peer match {
				case Failure(t)	=> Validated.invalid(t)
				case Success(t)	=> Validated.valid(t)
			}

		def toEitherT[F[_]:Applicative]:EitherT[F,Throwable,T]	=
			peer.toEither.toEitherT
	}
}
