package scutil.lang.extension

import scutil.lang.*

object PFunctionExtensions {
	implicit final class PFunctionExt[S,T](peer:S=>Option[T]) {
		def applyOrElse(it:S, default:T):T	=
			peer(it) getOrElse default

		def orDefault(default: =>T):Function1[S,T]	=
			orAlways(constant(default))

		def orAlways(that:Function[S,T]):Function1[S,T]	=
			it	=> peer(it) getOrElse that(it)

		def orElse(that:S=>Option[T]):S=>Option[T]	=
			it	=> peer(it) orElse that(it)

		/** symbolic alias for andThenFixed */
		def >=>[U](that:T=>Option[U]):S=>Option[U]	=
			this andThenFixed that

		/** symbolic alias for composeFixed */
		def <=<[R](that:R=>Option[S]):R=>Option[T]	=
			this composeFixed that

		def andThenFixed[U](that:T=>Option[U]):S=>Option[U]	=
			it	=> peer(it) flatMap that

		def composeFixed[R](that:R=>Option[S]):R=>Option[T]	=
			it	=> that(it) flatMap peer

		def toExtractor:Extractor[S,T]	=
			Extractor(peer)
	}
}
