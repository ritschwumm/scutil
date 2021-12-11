package scutil.gui.extension

import java.awt.Color

import scutil.color._
import scutil.gui.colorConversion

object RGBImplicits {
	implicit final class RGBExt(peer:RGB) {
		def toColor:Color	= colorConversion	RGB_Color(peer)
	}
}
