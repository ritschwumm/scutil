package scutil.security

import javax.security.auth.Destroyable

import scala.util.Using.Releasable

object instances {
	given DestroyableReleasable[T<:Destroyable]:Releasable[T]	= _.destroy()
}
