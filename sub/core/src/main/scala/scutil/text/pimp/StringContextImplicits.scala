package scutil.text.pimp

import scutil.text.Text

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
    implicit def toTextStringContextExt(peer:StringContext)	= new StringContextExt(peer)
}

final class StringContextExt(peer:StringContext) {
	/** provide a string interpolator "ss" that allows nothing but String in $ escapes */
	def so(args:String*):String	= peer.s(args:_*)
	
	/** applies Text.stripMarginOnly */
	def strip(args:String*):String	=
			Text stripMarginOnly peer.s(args:_*)
}
