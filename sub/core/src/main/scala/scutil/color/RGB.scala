package scutil.color

import java.lang.{ Integer => JInteger }
import java.util.regex.Pattern

import scala.math._

object RGB {
	val white	= RGB(1,1,1)
	val black	= RGB(0,0,0)
	
	private val patternHex	= Pattern compile "[0-9a-fA-F]{6}"
	
	def parseHex(s:String):Option[RGB]	=
			if ((patternHex matcher s).matches) {
				val Seq(r,g,b)	= (s grouped 2 map { it => (JInteger parseInt (it, 16))/ 255f }).toVector
				Some(RGB(r,g,b))
			}
			else None
		
	def fromIntRGB(argb:Int):RGB	=
			RGB(
				r	= ((argb >> 16) & 0xff) / 255f,
				g	= ((argb >>  8) & 0xff) / 255f,
				b	= ((argb >>  0) & 0xff) / 255f
			)
			
	def toIntARGB(rbg:RGB):Int	= rbg.toIntRGB
}

/** value range is 0..1 */
final case class RGB(r:Float, g:Float, b:Float) {
	def diff(that:RGB):Float	= 
			diff3(that) / 3f
			
	private[color] def diff3(that:RGB):Float	= 
			abs(this.r - that.r) +
			abs(this.g - that.g) + 
			abs(this.b - that.b)
			
	def withAlpha(alpha:Alpha):RGBA	=
			RGBA(this, alpha)
	
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
	
	def toIntRGB:Int	= 
			(((r * 255).toInt) << 16) |
			(((g * 255).toInt) <<  8) |
			(((b * 255).toInt) <<  0)
}
