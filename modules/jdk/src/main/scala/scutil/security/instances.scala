package scutil.security

import javax.security.auth.Destroyable

import scutil.lang.tc._

object instances extends instances

trait instances {
	implicit def DestroyableResource[T<:Destroyable]:Resource[T]	= Resource instance (_.destroy())
}
