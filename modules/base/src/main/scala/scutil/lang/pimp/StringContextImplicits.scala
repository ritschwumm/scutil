package scutil.lang.pimp

import scala.language.experimental.macros

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	implicit final class LangStringContextExt(peer:StringContext) {
		/** provide string interpolator for hex bytes */
		def byte():Byte		= macro HexNumberMacros.byteImpl
		/** provide string interpolator for hex bytes */
		def short():Short	= macro HexNumberMacros.shortImpl
		/** provide string interpolator for hex bytes */
		def int():Int		= macro HexNumberMacros.intImpl
		/** provide string interpolator for hex bytes */
		def long():Long		= macro HexNumberMacros.longImpl

		/** provide a string interpolator "so" that allows nothing but String in $ escapes */
		def so(args:String*):String		= peer.s(args:_*)

		/** provide a string interpolator "show" that requires a Show instance for all arguments */
		def show(args:Any*):String	= macro ShowMacros.showImpl
	}
}
