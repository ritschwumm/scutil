package scutil.security

import javax.security.auth.Destroyable

import scutil.lang.Resource

object instances extends instances

trait instances {
	implicit def DestroyableResource[T<:Destroyable]:Resource[T]	= Resource instance (_.destroy())
}
