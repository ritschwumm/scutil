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
final case class PBijection[S,T](get:PFunction[S,T], put:PFunction[T,S]) {
	@deprecated("0.127.0", "use get")
	def write	= get
	@deprecated("0.127.0", "use put")
	def read	= put
	
	// TODO optics add mod and modF etc.
	
	//------------------------------------------------------------------------------
	
	def inverse:PBijection[T,S]	=
			PBijection(put, get)
	
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
				u	=> that put	u flatMap this.put
			)
					
	@deprecated("0.127.0", "use this >=> that.toPBijection")
	def andThenBijection[U](that:Bijection[T,U]):PBijection[S,U]	=
			this >=> that.toPBijection
					
	@deprecated("0.127.0", "use this >=> that.toPBijection")
	def andThenPrism[U](that:Prism[T,U]):PBijection[S,U]	=
			this >=> that.toPBijection
		
	//------------------------------------------------------------------------------
			
	def toBijection:Bijection[Option[S],Option[T]]	=
			Bijection(_ flatMap get, _ flatMap put)
			
	def readExtractor:Extractor[T,S]	=
			Extractor(put)
		
	def writeExtractor:Extractor[S,T]	=
			Extractor(get)
}
