package scutil.lang

object assocRight {
	/** like the -> type, but right-associative */
	type ->:[A,B]	= (A,B)

	implicit final class AssocAnyExt[T](peer:T) {
		/** like the -> method, but right-associative */
		def ->: [S](that:S):(S,T)	= that -> peer
	}

	/** like the -> extractor, but right-associative */
	object ->: {
		def unapply[S,T](value:(S,T)):(S,T)	= value
	}
}
