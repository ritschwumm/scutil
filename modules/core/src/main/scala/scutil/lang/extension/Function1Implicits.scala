package scutil.lang.extension

import scutil.lang._

object Function1Implicits {
	implicit final class Function1Ext[S,T](peer:Function1[S,T]) {
		/** symbolic alias for apply */
		def <|(value:S):T	= peer(value)

		/** symbolic alias for andThen */
		def >=>[U](that:Function1[T,U]):Function1[S,U]	=
			peer andThen that

		/** symbolic alias for compose */
		def <=<[R](that:Function1[R,S]):Function1[R,T]	=
			peer compose that

		def map[U](func:T=>U):S=>U	=
			peer andThen func

		def flatMap[U](func:T=>S=>U):S=>U	=
			s	=> func(peer(s))(s)

		def contraMap[R](func:R=>S):R=>T	=
			peer compose func

		def partial(predicate:Predicate[S]):PartialFunction[S,T]	=
			new PartialFunction[S,T] {
				def isDefinedAt(s:S):Boolean	= predicate(s)
				def apply(s:S):T				= peer(s)
			}

		def toPartialFunction:PartialFunction[S,T]	=
			{ case x => peer(x) }

		def toPFunction:S=>Option[T]	=
			it => Some(peer(it))

		def toThunk(s:S):Thunk[T]	=
			() => peer(s)

		// standard lib only has this at arity 2 and above
		def curried:Function1[S,T]			= peer

		// standard lib only has this at arity 2 and above
		def tupled:Function1[Tuple1[S],T]	= it => peer(it._1)
	}
}
