package scutil.ext

object TraversableImplicits extends TraversableImplicits

trait TraversableImplicits {
	implicit def toTraversableExt[S](delegate:Traversable[S])	= new TraversableExt(delegate)
}

final class TraversableExt[S](delegate:Traversable[S]) {
	def forceSingle:S = {
		val	iterator	= delegate.toIterator
		require(iterator.hasNext, "expected exactly one element, found none")
		val	out			= iterator.next
		require(!iterator.hasNext, "expected exactly one element, found more")
		out
	}
}
