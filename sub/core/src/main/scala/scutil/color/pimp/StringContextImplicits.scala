package scutil.color.pimp

import scala.language.experimental.macros

import scutil.color._

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
    implicit def toColorStringContextExt(peer:StringContext)	= new StringContextExt(peer)
}

/** provide string interpolators for web-style hex colors */
final class StringContextExt(peer:StringContext) {
	def rgb():RGB	= macro HexColorMacros.rgbImpl
	def rgba():RGBA	= macro HexColorMacros.rgbaImpl
}
