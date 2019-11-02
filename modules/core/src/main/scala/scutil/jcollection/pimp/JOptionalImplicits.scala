package scutil.jcollection.pimp

import java.util.{ Optional => JOptional }

import scutil.lang.ISeq

object JOptionalImplicits extends JOptionalImplicits

trait JOptionalImplicits {
	implicit final class JOptionalExt[T](peer:JOptional[T]) {
		def cata[X](none: => X, some:T => X):X =
				if (peer.isPresent)	some(peer.get)
				else				none

		def toOption:Option[T]		= cata(None,			Some.apply)
		def toVector:Vector[T]		= cata(Vector.empty,	Vector(_))
		def toList:List[T]			= cata(List.empty,		List(_))
		@deprecated("use toSeq", "0.162.0")
		def toISeq:ISeq[T]			= toSeq
		def toSeq:Seq[T]			= toVector
		def toIterable:Iterable[T]	= toVector
		def toSet:Set[T]			= cata(Set.empty,		Set(_))
	}
}
