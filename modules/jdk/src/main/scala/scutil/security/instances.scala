package scutil.security

import javax.security.auth.Destroyable

import scala.util.Using.Releasable

object instances extends instances

trait instances {
	implicit def DestroyableReleasable[T<:Destroyable]:Releasable[T]	= _.destroy()
}
