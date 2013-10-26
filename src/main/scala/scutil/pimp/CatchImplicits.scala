package scutil.pimp

import scutil.lang._

import scala.util.control.Exception.Catch

object CatchImplicits extends CatchImplicits

trait CatchImplicits {
	implicit def toCatchExt[T](peer:Catch[T]) = new CatchExt[T](peer)
}

final class CatchExt[T](peer:Catch[T]) {
	def toTried:Catch[Tried[Throwable,T]]			= peer withApply Fail.apply
	def tried[U>:T](body: =>U):Tried[Throwable,U]	= toTried(Win(body))
}
