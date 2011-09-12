package scutil.ext

import scala.reflect.Manifest

object AnyRefImplicits extends AnyRefImplicits

trait AnyRefImplicits {
	implicit def toAnyRefExt[T <: AnyRef](delegate:T):AnyRefExt[T] = new AnyRefExt[T](delegate)
}

final class AnyRefExt[T <: AnyRef](delegate:T) {
	/** fail with an Exception if null */
	def nullError(s: =>String):T	= 
			if (delegate != null)	delegate 
			else					sys error s 
	
	/** Some if not null, None if null */
	def guardNotNull:Option[T]	=
			if (delegate != null)	Some(delegate)
			else					None
}
