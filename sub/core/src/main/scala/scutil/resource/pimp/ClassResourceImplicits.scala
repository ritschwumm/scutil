package scutil.resource.pimp

import scutil.resource._

object ClassResourceImplicits extends ClassResourceImplicits

trait ClassResourceImplicits {
	implicit def toClassResourceExt[T](peer:Class[T]) = new ClassResourceExt[T](peer)
}

final class ClassResourceExt[T](peer:Class[T]) {
	// NOTE paths are relative to the class unless preceeded by a slash
	def resources:ResourceProvider	=
			new ResourceProvider(path => Option(peer getResource path))
}
