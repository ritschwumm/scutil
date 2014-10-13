package scutil.text.pimp

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
    implicit def toTextStringContextExt(peer:StringContext)	= new StringContextExt(peer)
}

/** provide a string interpolator "ss" that allows nothing but String in $ escapes */
final class StringContextExt(peer:StringContext) {
	def ss(args:String*):String	= peer.s(args:_*)
}
