package scutil.gui.pimp

import java.awt.Color

import scutil.color._
import scutil.gui.colorConversion

object RGBImplicits extends RGBImplicits

trait RGBImplicits {
	implicit def toRGBExt(peer:RGB):RGBExt	= new RGBExt(peer)
}
	
final class RGBExt(peer:RGB) {
	def toColor:Color	=
			colorConversion	RGB_Color(peer)
}
