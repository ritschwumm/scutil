package scutil.text.pimp

import scutil.text._

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	implicit final class TextStringContextExt(peer:StringContext) {
		/** applies Text.stripMarginOnly */
		def strip(args:String*):String	= Text stripMarginOnly peer.s(args:_*)

		def tb(args:String*):String		= Block.generate(peer.parts map StringContext.processEscapes, args)
		def rtb(args:String*):String	= Block.generate(peer.parts, args)
	}
}
