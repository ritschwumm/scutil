package scutil.lang.extension

import scala.reflect.Selectable.reflectiveSelectable

object CloneableExtensions {
	type SafeCloneable	= Cloneable & { def clone():Object }

	extension [T <: SafeCloneable](peer:T) {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		def safeClone:T	=
			peer.clone().asInstanceOf[T]
	}
}
