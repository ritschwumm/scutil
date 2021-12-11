package scutil.gui

import java.awt.image.RGBImageFilter

import scutil.color.*

object ImageFilters {
	def rgba(func:RGBA=>RGBA):RGBImageFilter	=
		new RGBAFilter(func)

	def hsba(func:HSBA=>HSBA):RGBImageFilter	=
		rgba { rgba => func(rgba.toHSBA).toRGBA }

	def rgb(func:RGB=>RGB):RGBImageFilter	=
		rgba { rgba => rgba copy (rgb = func(rgba.rgb)) }

	def hsb(func:HSB=>HSB):RGBImageFilter	=
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
