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
}
