package scutil.lang

object PBijection {
	def partial[S,T](writeFunc:PartialFunction[S,T], readFunc:PartialFunction[T,S]):PBijection[S,T]	=
		PBijection[S,T](writeFunc.lift, readFunc.lift)

	def total[S,T](writeFunc:S=>T, readFunc:T=>S):PBijection[S,T]	=
		PBijection[S,T](writeFunc andThen Some.apply, readFunc andThen Some.apply)

	def identity[T]:PBijection[T,T]	=
		PBijection(Some.apply, Some.apply)

	def trivial[T]:PBijection[T,T]	=
		PBijection(constant(None), constant(None))
}

/** a partial Bijection */
final case class PBijection[S,T](get:S=>Option[T], set:T=>Option[S]) {
	// TODO optics add mod and modF etc.

	def inverse:PBijection[T,S]	=
		PBijection(set, get)

	/** symbolic alias for andThen */
	def >=>[U](that:PBijection[T,U]):PBijection[S,U]	=
		this andThen that

	/** symbolic alias for compose */
	def <=<[R](that:PBijection[R,S]):PBijection[R,T]	=
		this compose that

	def compose[R](that:PBijection[R,S]):PBijection[R,T]	=
		that andThen this

	def andThen[U](that:PBijection[T,U]):PBijection[S,U]	=
		PBijection(
			s	=> this get	s flatMap that.get,
			u	=> that set	u flatMap this.set
		)

	//------------------------------------------------------------------------------

	def toBijection:Bijection[Option[S],Option[T]]	=
		Bijection(_ flatMap get, _ flatMap set)

	def readExtractor:Extractor[T,S]	=
		Extractor(set)

	def writeExtractor:Extractor[S,T]	=
		Extractor(get)
}
