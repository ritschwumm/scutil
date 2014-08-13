package scutil.lang

import language.experimental.macros

object Fielding {
	implicit def provide[T]:Fielding[T]	= macro FielderImpl.compile[T]
}

case class Fielding[T](names:ISeq[String])
