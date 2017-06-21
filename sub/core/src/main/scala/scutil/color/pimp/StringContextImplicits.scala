package scutil.color.pimp

import scala.language.experimental.macros

import scutil.color._

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	/** provide string interpolators for web-style hex colors */
	implicit final class ColorStringContextExt(peer:StringContext) {
		def rgb():RGB	= macro HexColorMacros.rgbImpl
		def rgba():RGBA	= macro HexColorMacros.rgbaImpl
	}
}
