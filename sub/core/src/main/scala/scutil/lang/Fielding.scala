package scutil.lang

import language.experimental.macros

object Fielding {
	implicit def provide[T]:Fielding[T]	= macro FielderImpl.apply[T]
}

case class Fielding[T](names:Seq[String])
