package scutil.lang

import scutil.lang.tc._

object instances extends instances
trait instances {
	implicit def DisposableResource[T<:Disposable]:Resource[T]			= Resource by (_.dispose())
	implicit def AutoCloseableResource[T<:AutoCloseable]:Resource[T]	= Resource by (_.close())
}
