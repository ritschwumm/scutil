package scutil.lang

trait assocBase {
	/** allows building tuple types with -> just like values */
	type ->[A,B]	= (A,B)
	
	/** allows pattern matching on tuples constructed with -> */
	object -> {
		def unapply[S,T](value:(S,T)):Option[(S,T)]	= Some(value)
	}
}

// trait assocGenerated extends assocBase

object assoc extends assocGenerated
