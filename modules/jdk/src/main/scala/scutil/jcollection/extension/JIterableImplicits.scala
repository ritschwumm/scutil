package scutil.jcollection.extension

import java.lang.{
	Iterable	=> JIterable
}

object JIterableImplicits extends JIterableImplicits

trait JIterableImplicits {
	implicit final class JIterableExt[T](peer:JIterable[T]) {
		def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
	}
}
