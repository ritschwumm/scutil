package scutil.gui.extension

import java.awt.Color

object ColorExtensions {
	implicit final class ColorExt(peer:Color) {
		def withAlpha(alpha:Float):Color	=
			new Color(redF, greenF, blueF, alpha)

		def withoutAlpha:Color	=
			new Color(redF, greenF, blueF)

		def redF:Float		= peer.getRed	/ 255f
		def greenF:Float	= peer.getGreen	/ 255f
		def blueF:Float		= peer.getBlue	/ 255f
		def alphaF:Float	= peer.getAlpha	/ 255f
	}
}
