package scutil.gui.extension

import java.awt.Color

import scutil.color.*

import scutil.gui.colorConversion

object ColorConversionExtensions {
	extension (peer:Color) {
		def toRGB:RGB	= colorConversion.Color_RGB(peer)
		def toRGBA:RGBA	= colorConversion.Color_RGBA(peer)
		def toHSB:HSB	= colorConversion.Color_HSB(peer)
		def toHSBA:HSBA	= colorConversion.Color_HSBA(peer)
	}

	extension (peer:RGB) {
		def toColor:Color	= colorConversion.RGB_Color(peer)
	}

	extension (peer:RGBA) {
		def toColor:Color	= colorConversion.RGBA_Color(peer)
	}

	extension (peer:HSB) {
		def toColor:Color	= colorConversion.HSB_Color(peer)
	}

	extension (peer:HSBA) {
		def toColor:Color	= colorConversion.HSBA_Color(peer)
	}
}
