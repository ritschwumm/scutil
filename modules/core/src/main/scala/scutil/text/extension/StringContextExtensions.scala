package scutil.text.extension

import scutil.text.*

object StringContextExtensions {
	extension (peer:StringContext) {
		/** applies Text.stripMarginOnly */
		def strip(args:String*):String	= Text.stripMarginOnly(peer.s(args*))

		def tb(args:String*):String		= Block.generate(peer.parts.map(StringContext.processEscapes), args)
		def rtb(args:String*):String	= Block.generate(peer.parts, args)
	}
}
