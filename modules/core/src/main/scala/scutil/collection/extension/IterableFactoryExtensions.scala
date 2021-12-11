package scutil.collection.extension

import scala.collection.*

object IterableFactoryExtensions {
	implicit final class IterableFactoryExt[CC[_]](peer:IterableFactory[CC]) {
		def unfoldSimple[S,T<:S](seed:S)(func:S=>Option[T]):CC[T]	=
			peer.unfold(seed) { s =>
				func(s) map { t => (t,t) }
			}
	}
}
