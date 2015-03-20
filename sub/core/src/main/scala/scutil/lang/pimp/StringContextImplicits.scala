package scutil.lang.pimp

import scala.language.experimental.macros

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
    implicit def toLangStringContextExt(peer:StringContext)	= new StringContextExt(peer)
}

/** provide string interpolator for hex bytes */
final class StringContextExt(peer:StringContext) {
	def byte():Byte		= macro HexNumberMacros.byteImpl
	def short():Short	= macro HexNumberMacros.shortImpl
	def int():Int		= macro HexNumberMacros.intImpl
	def long():Long		= macro HexNumberMacros.longImpl
}
