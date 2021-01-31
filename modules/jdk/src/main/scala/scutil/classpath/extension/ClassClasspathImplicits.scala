package scutil.classpath.extension

import scutil.classpath._

object ClassClasspathImplicits extends ClassClasspathImplicits

trait ClassClasspathImplicits {
	implicit final class ClassResourceExt[T](peer:Class[T]) {
		// NOTE paths are relative to the class unless preceeded by a slash
		@deprecated("use classpathResource", "0.201.0")
		def classpathResourceProvider:ClasspathResourceProvider	=
			new ClasspathResourceProvider(path => Option(peer getResource path))

		/** paths are relative to the class unless preceeded by a slash */
		def classpathResource(path:String):Option[ClasspathResource]	=
			Option(peer getResource path) map ClasspathResource.apply

		/** paths are relative to the class unless preceeded by a slash */
		def classpathResourceOrError(path:String):ClasspathResource	=
			classpathResource(path) getOrElse sys.error(s"cannot find classpath resource ${path}")
	}
}
