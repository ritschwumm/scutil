package scutil.lang.pimp

import scala.util.control.Exception.Catch

object ExceptionCatchImplicits extends ExceptionCatchImplicits

trait ExceptionCatchImplicits {
	implicit final class ExceptionCatchExt[T](peer:Catch[T]) {
		def toEither:Catch[Either[Throwable,T]]			= peer withApply Left.apply
		def either[U>:T](body: =>U):Either[Throwable,U]	= toEither(Right(body))
	}
}
