package scutil.ext

import java.util.concurrent.Callable

object IteratorImplicits extends IteratorImplicits

trait IteratorImplicits {
	implicit def toIteratorExt[T](delegate:Iterator[T]) = new IteratorExt[T](delegate)
}

final class IteratorExt[T](delegate:Iterator[T]) {
	def nextOption:Option[T]	=
			if (delegate.hasNext)	Some(delegate.next)
			else					None
}
