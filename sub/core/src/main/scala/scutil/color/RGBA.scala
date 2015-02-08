package scutil.color

import java.lang.{ Integer => JInteger }
import java.util.regex.Pattern

object RGBA {
	val transparentBlack	= RGBA(RGB.black, Alpha.transparent)
	val transparentWhite	= RGBA(RGB.white, Alpha.transparent)
	
	private val patternHex	= Pattern compile "[0-9a-fA-F]{8}"
	
	def parseHex(s:String):Option[RGBA]	=
			if ((patternHex matcher s).matches) {
				val Seq(r,g,b,a)	= (s grouped 2 map { it => (JInteger parseInt (it, 16)) / 255f }).toVector
				Some(RGBA(RGB(r,g,b),Alpha(a)))
			}
			else None
			
	def fromIntARGB(argb:Int):RGBA	=
			RGBA(
				alpha	= Alpha(
					((argb >> 24) & 0xff) / 255f
				),
				rgb	= RGB(
					r	= ((argb >> 16) & 0xff) / 255f,
					g	= ((argb >>  8) & 0xff) / 255f,
					b	= ((argb >>  0) & 0xff) / 255f
				)
			)
			
	def toIntARGB(rbga:RGBA):Int	= rbga.toIntARGB
}

final case class RGBA(rgb:RGB, alpha:Alpha) {
	def r	= rgb.r
	def g	= rgb.g
	def b	= rgb.b
	def a	= alpha.a
	
	def diff(that:RGBA):Float	=
			(	(this.rgb	diff3	that.rgb) +
				(this.alpha	diff	that.alpha)
			) / 4f
	
	def toHSBA:HSBA =
			HSBA(rgb.toHSB, alpha)
	
	def toIntARGB:Int	=
			(((alpha.a	* 255).toInt) << 24) |
			(((rgb.r	* 255).toInt) << 16) |
			(((rgb.g	* 255).toInt) <<  8) |
			(((rgb.b	* 255).toInt) <<  0)
}
