package scutil.lang.pimp

import java.net.URL
import java.io.InputStream

object ClassImplicits extends ClassImplicits

trait ClassImplicits {
	implicit def toClassExt[T](peer:Class[T]) = new ClassExt[T](peer)
}

final class ClassExt[T](peer:Class[T]) {
	def resourceOption(path:String):Option[URL]					= Option(peer getResource path)
	def resourceAsStreamOption(path:String):Option[InputStream]	= Option(peer getResourceAsStream path)
	
	def boxed:Class[_]	= peer match {
		case java.lang.Boolean.TYPE		=> classOf[java.lang.Boolean]
		case java.lang.Character.TYPE	=> classOf[java.lang.Character]
		case java.lang.Byte.TYPE		=> classOf[java.lang.Byte]
		case java.lang.Short.TYPE		=> classOf[java.lang.Short]
		case java.lang.Integer.TYPE		=> classOf[java.lang.Integer]
		case java.lang.Long.TYPE		=> classOf[java.lang.Long]
		case java.lang.Float.TYPE		=> classOf[java.lang.Float]
		case java.lang.Double.TYPE		=> classOf[java.lang.Double]
		case x							=> x
	}
}
