package scutil.ext

import scala.reflect.Manifest

object AnyRefImplicits extends AnyRefImplicits

trait AnyRefImplicits {
	implicit def toAnyRefExt[T <: AnyRef](delegate:T):AnyRefExt[T] = new AnyRefExt[T](delegate)
}

final class AnyRefExt[T <: AnyRef](delegate:T) {
	def nullError(s: =>String):T	= 
			if (delegate != null)	delegate 
			else					sys error s 
	
	def guardNotNull:Option[T]	=
			if (delegate != null)	Some(delegate)
			else					None
			
	def instanceOption[T](implicit m:Manifest[T]):Option[T] =
			if (!(delegate eq null) && Manifest.singleType(delegate) <:< m)	Some(delegate.asInstanceOf[T])
			else															None
}
