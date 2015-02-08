package scutil.jcollection.pimp

import java.lang.{
	Iterable	=> JIterable
}

object JIterableImplicits extends JIterableImplicits

trait JIterableImplicits {
	implicit def toJIterableExt[T](peer:JIterable[T])	= new JIterableExt(peer)
}

final class JIterableExt[T](peer:JIterable[T]) {
	def toIterable:Iterable[T]	= new JIterableAsIterable(peer)
}
