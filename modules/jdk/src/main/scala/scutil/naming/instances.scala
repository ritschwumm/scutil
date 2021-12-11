package scutil.naming

import javax.naming._
import javax.naming.directory._

import scala.util.Using.Releasable

object instances {
	given DirContextReleasable[T<:DirContext]:Releasable[T]					= _.close()
	given NamingEnumerationReleasable[T<:NamingEnumeration[?]]:Releasable[T]	= _.close()
}
