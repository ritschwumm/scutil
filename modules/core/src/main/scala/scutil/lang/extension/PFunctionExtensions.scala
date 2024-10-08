package scutil.lang.extension

// import scala.annotation.targetName

import scutil.lang.*

object PFunctionExtensions {
	extension [S,T](peer:S=>Option[T]) {
		def applyOrElse(it:S, default:T):T	=
			peer(it).getOrElse(default)

		def orDefault(default: =>T):Function1[S,T]	=
			orAlways(constant(default))

		def orAlways(that:Function[S,T]):Function1[S,T]	=
			it	=> peer(it).getOrElse(that(it))

		def orElse(that:S=>Option[T]):S=>Option[T]	=
			it	=> peer(it) `orElse` that(it)

		/*
		// symbolic alias for andThenFixed
		@targetName("pFunctionAndThenFixed")
		def >=>[U](that:T=>Option[U]):S=>Option[U]	=
			andThenFixed(that)

		// symbolic alias for composeFixed
		@targetName("pFunctionComposeFixed")
		def <=<[R](that:R=>Option[S]):R=>Option[T]	=
			composeFixed(that)
		*/

		def andThenFixed[U](that:T=>Option[U]):S=>Option[U]	=
			it	=> peer(it).flatMap(that)

		def composeFixed[R](that:R=>Option[S]):R=>Option[T]	=
			it	=> that(it).flatMap(peer)

		def toExtractor:Extractor[S,T]	=
			Extractor(peer)
	}
}
