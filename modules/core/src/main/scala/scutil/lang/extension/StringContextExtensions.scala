package scutil.lang.extension

object StringContextExtensions {
	extension (peer:StringContext) {
		/** provide a string interpolator "so" that allows nothing but String in $ escapes */
		def so(args:String*):String		= peer.s(args*)
	}
}
