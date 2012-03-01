package scutil.gui

import java.awt.Color

import scala.math._

object HSBA {
	def fromColor(color:Color):HSBA	= RGBA fromColor color toHSBA;
}

/** value range is 0..1 */
case class HSBA(h:Float, s:Float, b:Float, a:Float) {
	def blend(that:HSBA, ratio:Float):HSBA	= {
		val iratio	= 1f - ratio
		HSBA(
			h	= this.h * ratio + that.h * iratio,
			s	= this.s * ratio + that.s * iratio,
			b	= this.b * ratio + that.b * iratio,
			a	= this.a * ratio + that.a * iratio
		)
	}
	
	def diff(that:HSBA):Float	=
			(	abs(this.h - that.h) +
				abs(this.s - that.s) + 
				abs(this.b - that.b) +
				abs(this.a - that.a)
			) / 4f
	
	def toRGBA:RGBA	= {
		if (s == 0)	return RGBA(b, b, b, a)
		
		val	H	= (h - floor(h).toFloat) * 6f
		val F	= H - floor(H).toFloat
		val P	= b * (1f - s)
		val Q	= b * (1f - s * F)
		val T	= b * (1f - (s * (1f - F)))
		H.toInt match {
			case 0	=> RGBA(b, T, P, a)
			case 1	=> RGBA(Q, b, P, a)
			case 2	=> RGBA(P, b, T, a)
			case 3	=> RGBA(P, Q, b, a)
			case 4	=> RGBA(T, P, b, a)
			case 5	=> RGBA(b, P, Q, a)
		}
    }
    
    def toColor:Color	= toRGBA.toColor 
}
