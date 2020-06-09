package scutil.resource.pimp

import scutil.resource._

object ClassResourceImplicits extends ClassResourceImplicits

trait ClassResourceImplicits {
	implicit final class ClassResourceExt[T](peer:Class[T]) {
		// NOTE paths are relative to the class unless preceeded by a slash
		def resourceProvider:ResourceProvider	=
			new ResourceProvider(path => Option(peer getResource path))
	}
}
