package scutil.lang.extension

import scala.util.{ Try, Success, Failure }

import scutil.core.implicits.*
import scutil.lang.*
import scutil.lang.tc.*

object TryExtensions {
	implicit final class TryExt[T](peer:Try[T]) {
		def cata[U](failure:Throwable=>U, success:T=>U):U	=
			peer.fold(failure, success)

		def toValidated:Validated[Throwable,T]	=
			peer.fold(Validated.invalid, Validated.valid)

		def toEitherT[F[_]:Applicative]:EitherT[F,Throwable,T]	=
			peer.toEither.toEitherT
	}
}
