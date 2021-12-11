package scutil.naming

import javax.naming.*
import javax.naming.directory.*

import scala.util.Using.Releasable

object instances {
	given DirContextReleasable[T<:DirContext]:Releasable[T]					= _.close()
	given NamingEnumerationReleasable[T<:NamingEnumeration[?]]:Releasable[T]	= _.close()
}
