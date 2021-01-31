package scutil.classpath.extension

import scutil.classpath._

object ClassLoaderClasspathImplicits extends ClassLoaderClasspathImplicits

trait ClassLoaderClasspathImplicits {
	implicit final class ClassLoaderResourceExt(peer:ClassLoader) {
		@deprecated("use classpathResourceProvider", "0.200.0")
		def resourceProvider:ClasspathResourceProvider	=
			classpathResourceProvider

		// NOTE paths here do not require a leading slash
		def classpathResourceProvider:ClasspathResourceProvider	=
			new ClasspathResourceProvider(path => Option(peer getResource path))
	}
}
