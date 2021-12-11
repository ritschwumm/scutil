package scutil.lang.extension

object StringContextExtensions {
	implicit final class LangStringContextExt(peer:StringContext) {
		/** provide a string interpolator "so" that allows nothing but String in $ escapes */
		def so(args:String*):String		= peer.s(args*)
	}
}
