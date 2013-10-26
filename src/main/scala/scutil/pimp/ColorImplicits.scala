package scutil.pimp

import scutil.color._

import java.awt.{ List=>AwtList, _ }

object ColorImplicits extends ColorImplicits

trait ColorImplicits {
	implicit def toColorExt(peer:Color):ColorExt	= new ColorExt(peer)
}
	
final class ColorExt(peer:Color) {
	def withAlpha(alpha:Float):Color	=
			new Color(peer.getRed, peer.getGreen, peer.getRed, alpha)
		
	def withoutAlpha:Color	=
			new Color(peer.getRed, peer.getGreen, peer.getRed)
		
	def toRGB:RGB	= RGB	fromColor peer
	def toRGBA:RGBA	= RGBA	fromColor peer
}
