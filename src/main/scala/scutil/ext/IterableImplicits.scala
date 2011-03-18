package scutil.ext

object IterableImplicits extends IterableImplicits

trait IterableImplicits {
	implicit def toIterableExt[T](delegate:Iterable[T])	= new IterableExt[T](delegate)
}

final class IterableExt[T](delegate:Iterable[T]) {
	//  list.view.compose(pf).head
	def collectHead[U](pf:PartialFunction[T,U]):Option[U] =
			delegate.view.collect(pf).headOption
}
