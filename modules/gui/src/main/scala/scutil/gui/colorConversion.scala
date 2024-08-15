package scutil.gui

import java.awt.Color

import scutil.color.*

object colorConversion {
	def RGB_Color(it:RGB):Color		= new Color(it.r, it.g, it.b)

	def Color_RGB(it:Color):RGB		=
		it.getRGBComponents(null) match {
			case Array(r, g, b, a)	=> RGB(r, g, b)
			case x					=> sys error s"unexpected number of rgb components: ${x.length.toString}"
		}

	def RGBA_Color(it:RGBA):Color	= new Color(it.rgb.r, it.rgb.g, it.rgb.b, it.alpha.a)

	def Color_RGBA(it:Color):RGBA	=
		it.getRGBComponents(null) match {
			case Array(r, g, b, a)	=> RGBA(RGB(r, g, b), Alpha(a))
			case x					=> sys error s"unexpected number of rgb components: ${x.length.toString}"
		}

	def HSB_Color(it:HSB):Color		= RGB_Color(it.toRGB)
	def Color_HSB(it:Color):HSB		= Color_RGB(it).toHSB

	def HSBA_Color(it:HSBA):Color	= RGBA_Color(it.toRGBA)
	def Color_HSBA(it:Color):HSBA	= Color_RGBA(it).toHSBA
}
