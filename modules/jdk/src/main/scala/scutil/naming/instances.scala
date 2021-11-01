package scutil.naming

import javax.naming._
import javax.naming.directory._

import scala.util.Using.Releasable

object instances extends instances

trait instances {
	implicit def DirContextReleasable[T<:DirContext]:Releasable[T]					= _.close()
	implicit def NamingEnumerationReleasable[T<:NamingEnumeration[?]]:Releasable[T]	= _.close()
}
