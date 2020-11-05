package scutil.resource.extension

import scutil.resource._

object ClassLoaderResourceImplicits extends ClassLoaderResourceImplicits

trait ClassLoaderResourceImplicits {
	implicit final class ClassLoaderResourceExt(peer:ClassLoader) {
		// NOTE paths here do not require a leading slash
		def resourceProvider:ResourceProvider	=
			new ResourceProvider(path => Option(peer getResource path))
	}
}
