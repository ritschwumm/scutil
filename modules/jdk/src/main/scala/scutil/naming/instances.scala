package scutil.naming

import javax.naming._
import javax.naming.directory._

import scutil.lang.Resource

object instances extends instances

trait instances {
	implicit def DirContextResource[T<:DirContext]:Resource[T]					= _.close()
	implicit def NamingEnumerationResource[T<:NamingEnumeration[_]]:Resource[T]	= _.close()
}
