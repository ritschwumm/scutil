package scutil.classpath.extension

import scutil.classpath.*

object ClassLoaderClasspathExtensions {
	implicit final class ClassLoaderResourceExt(peer:ClassLoader) {
		/** paths here do not require a leading slash */
		def classpathResource(path:String):Option[ClasspathResource]	=
			Option(peer getResource path) map ClasspathResource.apply

		/** paths here do not require a leading slash */
		def classpathResourceOrError(path:String):ClasspathResource	=
			classpathResource(path) getOrElse sys.error(s"cannot find classpath resource ${path}")
	}
}
