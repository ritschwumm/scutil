package scutil.gui.pimp

import java.awt.Color

import scutil.color._
import scutil.gui.colorConversion

object RGBAImplicits extends RGBAImplicits

trait RGBAImplicits {
	implicit def toRGBAExt(peer:RGBA):RGBAExt	= new RGBAExt(peer)
}
	
final class RGBAExt(peer:RGBA) {
	def toColor:Color	=
			colorConversion	RGBA_Color(peer)
}
