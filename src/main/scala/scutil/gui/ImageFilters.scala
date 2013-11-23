package scutil.gui

import java.awt.image.RGBImageFilter

import scutil.lang._
import scutil.color._

object ImageFilters {
	def rgba(func:Endo[RGBA]):RGBImageFilter	=
			new RGBAFilter(func)
	
	def hsba(func:Endo[HSBA]):RGBImageFilter	= 
			rgba { rgba => func(rgba.toHSBA).toRGBA }
		
	def rgb(func:Endo[RGB]):RGBImageFilter	= 
			rgba { rgba => rgba copy (rgb = func(rgba.rgb)) }
		
	def hsb(func:Endo[HSB]):RGBImageFilter	= 
			hsba { hsba => hsba copy (hsb = func(hsba.hsb)) }
}

private final class RGBAFilter(func:RGBA=>RGBA) extends RGBImageFilter {
	canFilterIndexColorModel = true
	
	override def filterRGB(x:Int, y:Int, argb:Int):Int = {
		val input		= RGBA fromIntARGB argb
		val filtered	= func(input)
		val output		= RGBA toIntARGB filtered
		output
	}
}
