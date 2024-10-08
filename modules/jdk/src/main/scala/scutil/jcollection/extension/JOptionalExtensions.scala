package scutil.jcollection.extension

import java.util.{ Optional as JOptional }

object JOptionalExtensions {
	extension [T](peer:JOptional[T]) {
		def cata[X](none: => X, some:T => X):X =
			if (peer.isPresent)	some(peer.get)
			else				none

		def toOption:Option[T]		= cata(None,			Some.apply)
		def toVector:Vector[T]		= cata(Vector.empty,	Vector(_))
		def toList:List[T]			= cata(List.empty,		List(_))
		def toSeq:Seq[T]			= toVector
		def toIterable:Iterable[T]	= toVector
		def toSet:Set[T]			= cata(Set.empty,		Set(_))
	}
}
