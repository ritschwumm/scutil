package scutil.lang.pimp

import scutil.io.ResourceProvider

object ClassLoaderImplicits extends ClassLoaderImplicits

trait ClassLoaderImplicits {
	implicit def toClassLoaderExt(peer:ClassLoader) = new ClassLoaderExt(peer)
}

final class ClassLoaderExt(peer:ClassLoader) {
	// NOTE paths here do not require a leading slash
	def resources:ResourceProvider	=
			new ResourceProvider(path => Option(peer getResource path))
}
