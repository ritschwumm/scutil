package scutil.text.pimp

import scutil.text.Text

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	implicit final class TextStringContextExt(peer:StringContext) {
		/** applies Text.stripMarginOnly */
		def strip(args:String*):String	= Text stripMarginOnly peer.s(args:_*)
	}
}
