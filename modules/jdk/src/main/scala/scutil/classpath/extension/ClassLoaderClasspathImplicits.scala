package scutil.classpath.extension

import scutil.classpath._

object ClassLoaderClasspathImplicits extends ClassLoaderClasspathImplicits

trait ClassLoaderClasspathImplicits {
	implicit final class ClassLoaderResourceExt(peer:ClassLoader) {
		// NOTE paths here do not require a leading slash
		@deprecated("use classpathResource", "0.201.0")
		def classpathResourceProvider:ClasspathResourceProvider	=
			new ClasspathResourceProvider(path => Option(peer getResource path))

		/** paths here do not require a leading slash */
		def classpathResource(path:String):Option[ClasspathResource]	=
			Option(peer getResource path) map ClasspathResource.apply

		/** paths here do not require a leading slash */
		def classpathResourceOrError(path:String):ClasspathResource	=
			classpathResource(path) getOrElse sys.error(s"cannot find classpath resource ${path}")
	}
}
