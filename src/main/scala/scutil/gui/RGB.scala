package scutil.gui

import java.awt.Color

import scala.math._

object RGB {
	/** ignores the alpha channel */
	def fromColor(color:Color):RGB	= {
		val Array(r,g,b,a)	= color getRGBComponents null
		RGB(r,g,b)
	}
}

/** value range is 0..1 */
case class RGB(r:Float, g:Float, b:Float) {
	def blend(that:RGB, ratio:Float):RGB	= {
		val iratio	= 1f - ratio
		RGB(
			(this.r * ratio + that.r * iratio),
			(this.g * ratio + that.g * iratio),
			(this.b * ratio + that.b * iratio)
		)
	}
	
	def diff(that:RGB):Float	= 
			(	abs(this.r - that.r) +
				abs(this.g - that.g) + 
				abs(this.b - that.b)
			) / 3f
	
	def toHSB:HSB = {
		val	cmax	= r max g max b
		val	cmin	= r min g min b
		val	B	= cmax
		if (B == 0)	return HSB(0,0,B)
		
		val cdiff	= (cmax - cmin)
		val	S	= cdiff / cmax
		if (S == 0)	return HSB(0,S,B)
		
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
		HSB(H,S,B)
	}
	
	/** fully sets the alpha channel */
	def toColor:Color	= new Color(r,g,b)
}
