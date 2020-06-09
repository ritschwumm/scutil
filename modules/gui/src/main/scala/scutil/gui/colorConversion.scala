package scutil.gui

import java.awt.Color

import scutil.color._

object colorConversion {
	def RGB_Color(it:RGB):Color		= new Color(it.r, it.g, it.b)
	def Color_RGB(it:Color):RGB		= {
		val Array(r, g, b, a)	= it getRGBComponents null
		RGB(r, g, b)
	}

	def RGBA_Color(it:RGBA):Color	= new Color(it.rgb.r, it.rgb.g, it.rgb.b, it.alpha.a)
	def Color_RGBA(it:Color):RGBA	= {
		val Array(r, g, b, a)	= it getRGBComponents null
		RGBA(RGB(r, g, b), Alpha(a))
	}

	def HSB_Color(it:HSB):Color		= RGB_Color(it.toRGB)
	def Color_HSB(it:Color):HSB		= Color_RGB(it).toHSB

	def HSBA_Color(it:HSBA):Color	= RGBA_Color(it.toRGBA)
	def Color_HSBA(it:Color):HSBA	= Color_RGBA(it).toHSBA
}
