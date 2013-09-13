package scutil.pimp

import scutil.lang._

import scala.util.control.Exception.Catch

object CatchImplicits extends CatchImplicits

trait CatchImplicits {
	implicit def toCatchExt[T](delegate:Catch[T]) = new CatchExt[T](delegate)
}

final class CatchExt[T](delegate:Catch[T]) {
	def toTried:Catch[Tried[Throwable,T]]			= delegate withApply Fail.apply
	def tried[U>:T](body: =>U):Tried[Throwable,U]	= toTried(Win(body))
}
