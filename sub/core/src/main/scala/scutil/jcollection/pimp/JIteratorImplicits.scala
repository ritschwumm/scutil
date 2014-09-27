package scutil.jcollection.pimp

import java.util.{ Iterator => JIterator }

object JIteratorImplicits extends JIteratorImplicits

trait JIteratorImplicits {
	implicit def toJIteratorExt[T](peer:JIterator[T])	= new JIteratorExt(peer)
}

final class JIteratorExt[T](peer:JIterator[T]) {
	def toIterator:Iterator[T] = new JIteratorWrapper(peer)
}
