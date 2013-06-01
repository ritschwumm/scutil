package scutil.color

import java.awt.Color

import scala.math._

import scutil.Math

object RGB {
	def fromColor(color:Color):RGB	= {
		val Array(r, g, b, a)	= color getRGBComponents null
		RGB(r, g, b)
	}
	
	def toColor(rbg:RGB):Color	= rbg.toColor
}

/** value range is 0..1 */
case class RGB(r:Float, g:Float, b:Float) {
	def blend(that:RGB, ratio:Float):RGB	= 
			RGB(
				r	= Math blend (this.r, that.r, ratio),
				g	= Math blend (this.g, that.g, ratio),
				b	= Math blend (this.b, that.b, ratio)
			)
	
	def diff(that:RGB):Float	= 
			diff3(that) / 3f
			
	private[color] def diff3(that:RGB):Float	= 
			abs(this.r - that.r) +
			abs(this.g - that.g) + 
			abs(this.b - that.b)
	
	def toHSB:HSB = {
		val	cmax	= r max g max b
		val	cmin	= r min g min b
		val	B	= cmax
		if (B == 0)	return HSB(0, 0, B)
		
		val cdiff	= (cmax - cmin)
		val	S	= cdiff / cmax
		if (S == 0)	return HSB(0, S, B)
		
		val	rc	= (cmax - r) / cdiff
		val	gc	= (cmax - g) / cdiff
		val	bc	= (cmax - b) / cdiff
		
		val hh	=	
					 if (r == cmax)	bc - gc
				else if (g == cmax)	2f + rc - bc
				else 				4f + gc - rc
		val	h	= hh / 6f
		val	H	= 
				if (h < 0)	h + 1f
				else 		h
		HSB(H, S, B)
	}
	
	def toColor:Color	= 
			new Color(r, g, b)
}
