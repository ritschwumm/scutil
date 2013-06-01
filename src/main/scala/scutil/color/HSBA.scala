package scutil.color

import java.awt.Color

import scala.math._

import scutil.Math

object HSBA {
	def fromColor(color:Color):HSBA	= (RGBA fromColor color).toHSBA
	def toColor(hsba:HSBA):Color	= hsba.toColor
	
	def fromIntARGB(argb:Int):HSBA	= (RGBA fromIntARGB argb).toHSBA
	def toIntARGB(hsba:HSBA):Int	= hsba.toIntARGB
}

case class HSBA(hsb:HSB, alpha:Alpha) {
	def h	= hsb.h
	def s	= hsb.s
	def b	= hsb.b
	def a	= alpha.a
	
	def blend(that:HSBA, ratio:Float):HSBA	= 
			HSBA(
				hsb		= this.hsb		blend (that.hsb,	ratio),
				alpha	= this.alpha	blend (that.alpha,	ratio)
			)
	
	def diff(that:HSBA):Float	=
			(	(this.hsb	diff3	that.hsb) +
				(this.alpha	diff	that.alpha)
			) / 4f
	
	def toRGBA:RGBA	=
			RGBA(hsb.toRGB, alpha)
    
    def toColor:Color	= 
    		toRGBA.toColor 
    	
    def toIntARGB:Int	= 
    		toRGBA.toIntARGB 
}
