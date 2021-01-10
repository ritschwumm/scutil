package scutil.collection.extension

import scala.collection._

object IterableFactoryImplicits extends IterableFactoryImplicits

trait IterableFactoryImplicits {
	implicit final class IterableFactoryImplicitsExt[CC[_]](peer:IterableFactory[CC]) {
		def unfoldSimple[S,T<:S](seed:S)(func:S=>Option[T]):CC[T]	=
			peer.unfold(seed) { s =>
				func(s) map { t => (t,t) }
			}
	}
}
