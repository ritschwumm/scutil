package scutil.security

import javax.security.auth.Destroyable

import scala.util.Using.Releasable

object instances {
	given [T<:Destroyable] => Releasable[T]	= _.destroy()
}
