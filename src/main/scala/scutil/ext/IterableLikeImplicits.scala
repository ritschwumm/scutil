package scutil.ext

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

object IterableLikeImplicits extends IterableLikeImplicits

trait IterableLikeImplicits {
	implicit def toIterableLikeExt[T,C[T]<:IterableLike[T,C[T]]](delegate:C[T]) = new IterableLikeExt[T,C](delegate)
}

final class IterableLikeExt[T,C[T]<:IterableLike[T,C[T]]](delegate:C[T]) {
	/** combine elements of two collections using a function */
	def zipWith[U,V,That](that:Iterable[U])(f:(T,U)=>V)(implicit cbf:CanBuildFrom[C[T],V,That]):That	= {
		val builder	= cbf(delegate.repr)
		val	xi	= delegate.iterator
		val yi	= that.iterator
		while (xi.hasNext && yi.hasNext) {
			builder	+= f(xi.next, yi.next)
		}
		builder.result
	}
}
