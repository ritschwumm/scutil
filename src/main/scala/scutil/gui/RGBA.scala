package scutil.gui

import java.awt.Color

import scala.math._

import scutil.Math

object RGBA {
	def fromColor(color:Color):RGBA	= {
		val Array(r,g,b,a)	= color getRGBComponents null
		RGBA(r,g,b,a)
	}
	
	def fromIntARGB(argb:Int):RGBA	= RGBA(
			a	= ((argb >> 24) & 0xff) / 255f,
			r	= ((argb >> 16) & 0xff) / 255f,
			g	= ((argb >>  8) & 0xff) / 255f,
			b	= ((argb >>  0) & 0xff) / 255f)
}

/** value range is 0..1 */
case class RGBA(r:Float, g:Float, b:Float, a:Float) {
	def blend(that:RGBA, ratio:Float):RGBA	= 
			RGBA(
				r	= Math blend (this.r, that.r, ratio),
				g	= Math blend (this.g, that.g, ratio),
				b	= Math blend (this.b, that.b, ratio),
				a	= Math blend (this.a, that.a, ratio)
			)
	
	def diff(that:RGBA):Float	= 
			(	abs(this.r - that.r) +
				abs(this.g - that.g) + 
				abs(this.b - that.b) +
				abs(this.a - that.a)
			) / 4f
	
	def toHSBA:HSBA = {
		val	cmax	= r max g max b
		val	cmin	= r min g min b
		val	B	= cmax
		if (B == 0)	return HSBA(0,0,B,a)
		
		val cdiff	= (cmax - cmin)
		val	S	= cdiff / cmax
		if (S == 0)	return HSBA(0,S,B,a)
		
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
		HSBA(H,S,B,a)
	}
	
	def toColor:Color	= new Color(r,g,b,a)
	
	def toIntARGB:Int	= 
			(((a * 255).toInt) << 24) |
			(((r * 255).toInt) << 16) |
			(((g * 255).toInt) <<  8) |
			(((b * 255).toInt) <<  0)
}
