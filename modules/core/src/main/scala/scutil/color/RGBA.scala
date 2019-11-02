package scutil.color

import scutil.lang._
import scutil.codec._

object RGBA {
	val transparentBlack	= RGBA(RGB.black, Alpha.transparent)
	val transparentWhite	= RGBA(RGB.white, Alpha.transparent)

	def parseHex(s:String):Option[RGBA]	=
			Hex decodeByteString s collect { case ByteString(r,g,b,a)	=>
				RGBA(
					RGB(
						(r & 0xff) / 255f,
						(g & 0xff) / 255f,
						(b & 0xff) / 255f
					),
					Alpha(
						(a & 0xff) / 255f
					)
				)
			}

	def unparseHex(rgba:RGBA):String	= rgba.unparseHex

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

	def unparseHex:String	=
			Hex encodeByteString ByteString(
				(r			* 255).toByte,
				(g			* 255).toByte,
				(b			* 255).toByte,
				(alpha.a	* 255).toByte
			)
}
