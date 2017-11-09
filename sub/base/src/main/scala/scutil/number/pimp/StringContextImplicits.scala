package scutil.number.pimp

import scala.language.experimental.macros

import scutil.number.BigRational

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	implicit final class NumberStringContextExt(peer:StringContext) {
		def br():BigRational	= macro NumberMacros.brImpl
	}
}
