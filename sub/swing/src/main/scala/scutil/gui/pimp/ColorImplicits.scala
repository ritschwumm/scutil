package scutil.gui.pimp

import scutil.color._

import java.awt.Color

import scutil.gui.colorConversion

object ColorImplicits extends ColorImplicits

trait ColorImplicits {
	implicit def toColorExt(peer:Color):ColorExt	= new ColorExt(peer)
}
	
final class ColorExt(peer:Color) {
	def withAlpha(alpha:Float):Color	=
			new Color(peer.getRed, peer.getGreen, peer.getRed, alpha)
		
	def withoutAlpha:Color	=
			new Color(peer.getRed, peer.getGreen, peer.getRed)
		
	def toRGB:RGB	= colorConversion Color_RGB		peer
	def toRGBA:RGBA	= colorConversion Color_RGBA	peer
	def toHSB:HSB	= colorConversion Color_HSB		peer
	def toHSBA:HSBA	= colorConversion Color_HSBA	peer
}
