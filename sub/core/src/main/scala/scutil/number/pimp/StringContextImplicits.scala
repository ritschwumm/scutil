package scutil.number.pimp

import scala.language.experimental.macros

import scutil.number.BigRational

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
    implicit def toMathStringContextExt(peer:StringContext)	= new StringContextExt(peer)
}

final class StringContextExt(peer:StringContext) {
	def br():BigRational	= macro NumberMacros.brImpl
}
