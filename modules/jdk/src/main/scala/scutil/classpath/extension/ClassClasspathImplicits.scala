package scutil.classpath.extension

import scutil.classpath._

object ClassClasspathImplicits extends ClassClasspathImplicits

trait ClassClasspathImplicits {
	implicit final class ClassResourceExt[T](peer:Class[T]) {
		@deprecated("use classpathResourceProvider", "0.200.0")
		def resourceProvider:ClasspathResourceProvider	=
			classpathResourceProvider

		// NOTE paths are relative to the class unless preceeded by a slash
		def classpathResourceProvider:ClasspathResourceProvider	=
			new ClasspathResourceProvider(path => Option(peer getResource path))
	}
}
