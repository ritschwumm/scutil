package scutil.color

import java.awt.Color

import scala.math._

import scutil.Math

object HSB {
	def fromColor(color:Color):HSB	= (RGB fromColor color).toHSB
	def toColor(hsb:HSB):Color		= hsb.toColor
}

/** value range is 0..1 */
case class HSB(h:Float, s:Float, b:Float) {
	def blend(that:HSB, ratio:Float):HSB	= 
			HSB(
				h	= Math blend (this.h, that.h, ratio),
				s	= Math blend (this.s, that.s, ratio),
				b	= Math blend (this.b, that.b, ratio)
			)
	
	def diff(that:HSB):Float	=
			diff3(that) / 3f
			
	private[color] def diff3(that:HSB):Float	=
			abs(this.h - that.h) +
			abs(this.s - that.s) +
			abs(this.b - that.b)
	
	def toRGB:RGB	= {
		if (s == 0)	return RGB(b, b, b)
		
		val	H	= (h - floor(h).toFloat) * 6f
		val F	= H - floor(H).toFloat
		val P	= b * (1f - s)
		val Q	= b * (1f - s * F)
		val T	= b * (1f - (s * (1f - F)))
		H.toInt match {
			case 0	=> RGB(b, T, P)
			case 1	=> RGB(Q, b, P)
			case 2	=> RGB(P, b, T)
			case 3	=> RGB(P, Q, b)
			case 4	=> RGB(T, P, b)
			case 5	=> RGB(b, P, Q)
		}
    }
    
    def toColor:Color	= 
    		toRGB.toColor 
}
