package scutil.pimp

import scutil.lang._

import scala.util.control.Exception.Catch

object ExceptionCatchImplicits extends ExceptionCatchImplicits

trait ExceptionCatchImplicits {
	implicit def toExceptionCatchExt[T](peer:Catch[T]) = new ExceptionCatchExt[T](peer)
}

final class ExceptionCatchExt[T](peer:Catch[T]) {
	def toTried:Catch[Tried[Throwable,T]]			= peer withApply Fail.apply
	def tried[U>:T](body: =>U):Tried[Throwable,U]	= toTried(Win(body))
}
