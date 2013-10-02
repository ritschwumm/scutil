package scutil.lang

object PBijection {
	def partial[S,T](writeFunc:PartialFunction[S,T], readFunc:PartialFunction[T,S]):PBijection[S,T]	= 
			PBijection[S,T](writeFunc.lift, readFunc.lift)
			
	def identity[T]:PBijection[T,T]	= 
			PBijection(Some.apply, Some.apply)
			
	def trivial[T]:PBijection[T,T]	=
			PBijection(constant(None), constant(None))
}

/** a partial Bijection */
final case class PBijection[S,T](write:PFunction[S,T], read:PFunction[T,S]) {
	def inverse:PBijection[T,S]	= 
			PBijection(read, write)
	
	/** symbolic alias for andThen */
	@inline
	def >=>[U](that:PBijection[T,U]):PBijection[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	@inline
	def <=<[R](that:PBijection[R,S]):PBijection[R,T]	= 
			this compose that
		
	def compose[R](that:PBijection[R,S]):PBijection[R,T]	= 
			that andThen this
	
	def andThen[U](that:PBijection[T,U]):PBijection[S,U]	=
			PBijection(
					s	=> this write	s flatMap that.write,
					u	=> that read	u flatMap this.read)
			
	def asBijection:Bijection[Option[S],Option[T]]	=
			Bijection(_ flatMap write, _ flatMap read)
			
	def readExtractor:Extractor[T,S]	=
			Extractor(read)
		
	def writeExtractor:Extractor[S,T]	= 
			Extractor(write)
}
