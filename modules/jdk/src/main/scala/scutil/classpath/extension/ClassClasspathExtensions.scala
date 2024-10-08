package scutil.classpath.extension

import scutil.classpath.*

object ClassClasspathExtensions {
	extension [T](peer:Class[T]) {
		/** paths are relative to the class unless preceeded by a slash */
		def classpathResource(path:String):Option[ClasspathResource]	=
			Option(peer.getResource(path)).map(ClasspathResource.apply)

		/** paths are relative to the class unless preceeded by a slash */
		def classpathResourceOrError(path:String):ClasspathResource	=
			classpathResource(path).getOrElse{
				sys.error(s"cannot find classpath resource ${path}")
			}
	}
}
