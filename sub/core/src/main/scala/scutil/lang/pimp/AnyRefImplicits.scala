package scutil.lang.pimp

object AnyRefImplicits extends AnyRefImplicits

trait AnyRefImplicits {
	implicit def toAnyRefExt[T <: AnyRef](peer:T):AnyRefExt[T] = new AnyRefExt[T](peer)
}

final class AnyRefExt[T <: AnyRef](peer:T) {
	/** Some if not null, None if null */
	def guardNotNull:Option[T]	=
			if (peer != null)	Some(peer)
			else				None
	
	/** replace null with another value */
	def replaceNull[U>:T](replacement: =>U):U	=
			if (peer != null)	peer
			else				replacement
	
	/** fail with an Exception if null */
	def nullError(s: =>String):T	=
			if (peer != null)	peer
			else				sys error s
}
