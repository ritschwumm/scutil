package scutil.gui.extension

import java.awt.Color

import scutil.color.*

import scutil.gui.colorConversion

object ColorConversionExtensions {
	implicit final class ColorConversionColorExt(peer:Color) {
		def toRGB:RGB	= colorConversion Color_RGB		peer
		def toRGBA:RGBA	= colorConversion Color_RGBA	peer
		def toHSB:HSB	= colorConversion Color_HSB		peer
		def toHSBA:HSBA	= colorConversion Color_HSBA	peer
	}

	implicit final class ColorConversionRGBExt(peer:RGB) {
		def toColor:Color	= colorConversion	RGB_Color(peer)
	}

	implicit final class ColorConversionRGBAExt(peer:RGBA) {
		def toColor:Color	= colorConversion	RGBA_Color(peer)
	}

	implicit final class ColorConversionHSBExt(peer:HSB) {
		def toColor:Color	= colorConversion	HSB_Color(peer)
	}

	implicit final class ColorConversionHSBAExt(peer:HSBA) {
		def toColor:Color	= colorConversion	HSBA_Color(peer)
	}
}
