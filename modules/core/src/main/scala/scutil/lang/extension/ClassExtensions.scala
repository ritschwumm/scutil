package scutil.lang.extension

object ClassExtensions {
	implicit final class ClassExt[T](peer:Class[T]) {
		def boxed:Class[?]	= peer match {
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
}
