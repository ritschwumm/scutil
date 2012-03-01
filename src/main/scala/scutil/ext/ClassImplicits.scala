package scutil.ext

import java.net.URL
import java.io.InputStream

object ClassImplicits extends ClassImplicits

trait ClassImplicits {
	implicit def toClassExt[T](delegate:Class[T]) = new ClassExt[T](delegate)
}

final class ClassExt[T](delegate:Class[T]) {
	def resourceOption(path:String):Option[URL]					= Option(delegate getResource path)
	def resourceAsStreamOption(path:String):Option[InputStream]	= Option(delegate getResourceAsStream path)
	
	def boxed:Class[_]	= delegate match {
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
