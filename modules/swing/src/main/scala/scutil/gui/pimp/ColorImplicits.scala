package scutil.gui.pimp

import scutil.color._

import java.awt.Color

import scutil.gui.colorConversion

object ColorImplicits extends ColorImplicits

trait ColorImplicits {
	implicit final class ColorExt(peer:Color) {
		def withAlpha(alpha:Float):Color	=
			new Color(redF, greenF, blueF, alpha)

		def withoutAlpha:Color	=
			new Color(redF, greenF, blueF)

		def redF:Float		= peer.getRed	/ 255f
		def greenF:Float	= peer.getGreen	/ 255f
		def blueF:Float		= peer.getBlue	/ 255f
		def alphaF:Float	= peer.getAlpha	/ 255f

		def toRGB:RGB	= colorConversion Color_RGB		peer
		def toRGBA:RGBA	= colorConversion Color_RGBA	peer
		def toHSB:HSB	= colorConversion Color_HSB		peer
		def toHSBA:HSBA	= colorConversion Color_HSBA	peer
	}
}
