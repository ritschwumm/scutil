package scutil.lang

trait assocBase {
	/** allows building tuple types with -> just like values */
	type ->[A,B]	= (A,B)

	/** allows pattern matching on tuples constructed with -> */
	object -> {
		def unapply[S,T](value:(S,T)):Some[(S,T)]	= Some(value)
	}

	//------------------------------------------------------------------------------
	//## right-associative variant of ->

	/** like the -> type, but right-associative */
	type ->:[A,B]	= (A,B)

	implicit final class AssocAnyExt[T](peer:T) {
		/** like the -> method, but right-associative */
		def ->: [S](that:S):(S,T)	= that -> peer
	}

	/** like the -> extractor, but right-associative */
	object ->: {
		def unapply[S,T](value:(S,T)):Some[(S,T)]	= Some(value)
	}
}

// trait assocGenerated extends assocBase

object assoc extends assocGenerated
