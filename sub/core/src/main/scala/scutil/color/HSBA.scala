package scutil.color

object HSBA {
	val transparentBlack	= HSBA(HSB.black, Alpha.transparent)
	val transparentWhite	= HSBA(HSB.white, Alpha.transparent)
	
	def fromIntARGB(argb:Int):HSBA	= (RGBA fromIntARGB argb).toHSBA
	def toIntARGB(hsba:HSBA):Int	= hsba.toIntARGB
}

final case class HSBA(hsb:HSB, alpha:Alpha) {
	def h	= hsb.h
	def s	= hsb.s
	def b	= hsb.b
	def a	= alpha.a
	
	def diff(that:HSBA):Float	=
			(	(this.hsb	diff3	that.hsb) +
				(this.alpha	diff	that.alpha)
			) / 4f
	
	def toRGBA:RGBA	=
			RGBA(hsb.toRGB, alpha)
	
    def toIntARGB:Int	=
    		toRGBA.toIntARGB
}
