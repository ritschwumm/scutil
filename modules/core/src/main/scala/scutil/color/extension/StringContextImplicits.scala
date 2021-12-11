package scutil.color.extension

import scutil.color._

object StringContextImplicits {
	/** provide string interpolators for web-style hex colors */
	extension (inline peer:StringContext) {
		inline def rgb(inline parts:Any*):RGB	= ${ HexColorMacros.rgb('peer) }
		inline def rgba(inline parts:Any*):RGBA	= ${ HexColorMacros.rgba('peer) }
	}
}
