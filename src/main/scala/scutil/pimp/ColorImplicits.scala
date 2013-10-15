package scutil.pimp

import scutil.color._

import java.awt.{ List=>AwtList, _ }

object ColorImplicits extends ColorImplicits

trait ColorImplicits {
	implicit def toColorExt(delegate:Color):ColorExt	= new ColorExt(delegate)
}
	
final class ColorExt(delegate:Color) {
	def withAlpha(alpha:Float):Color	=
			new Color(delegate.getRed, delegate.getGreen, delegate.getRed, alpha)
		
	def withoutAlpha:Color	=
			new Color(delegate.getRed, delegate.getGreen, delegate.getRed)
		
	def toRGB:RGB	= RGB	fromColor delegate
	def toRGBA:RGBA	= RGBA	fromColor delegate
}
