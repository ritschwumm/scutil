package scutil.lang.extension

object StringContextImplicits {
	implicit final class LangStringContextExt(peer:StringContext) {
		/** provide a string interpolator "so" that allows nothing but String in $ escapes */
		def so(args:String*):String		= peer.s(args:_*)
	}

	extension (inline peer:StringContext) {
		/** provide string interpolator for hex bytes */
		inline def byte(inline parts:Any*):Byte		= ${ HexNumberMacros.byte('peer) }

		/** provide string interpolator for hex bytes */
		inline def short(inline parts:Any*):Short	= ${ HexNumberMacros.short('peer) }

		/** provide string interpolator for hex bytes */
		inline def int(inline parts:Any*):Int		= ${ HexNumberMacros.int('peer) }

		/** provide string interpolator for hex bytes */
		inline def long(inline parts:Any*):Long		= ${ HexNumberMacros.long('peer) }

		/** provide a string interpolator "show" that requires a Show instance for all arguments */
		inline def show(inline parts:Any*):String	= ${ ShowMacros.show('peer, 'parts) }
	}
}
