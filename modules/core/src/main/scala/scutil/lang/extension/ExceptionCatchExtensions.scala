package scutil.lang.extension

import scala.util.control.Exception.Catch

object ExceptionCatchExtensions {
	extension [T](peer:Catch[T]) {
		def toEither:Catch[Either[Throwable,T]]			= peer withApply Left.apply
		def either[U>:T](body: =>U):Either[Throwable,U]	= toEither(Right(body))
	}
}
