package scutil.lang

object Extractor {
	def total[S,T](func:S=>T):Extractor[S,T]	=
			Extractor(s	=> Some(func(s)))
	
	def partial[S,T](func:PartialFunction[S,T]):Extractor[S,T]	=
			Extractor(s	=> if (func isDefinedAt s) Some(func apply s) else None)
	
	def guarding[T](pred:Predicate[T]):Extractor[T,T]	=
			Extractor(it => if (pred(it)) Some(it) else None)
			
	def identity[T]:Extractor[T,T]	= 
			Extractor(Some.apply)
			
	def trivial[T]:Extractor[T,Any]	= 
			Extractor(constant(None))
}
	
/** representative extractor (as opposed to compiler magic) */
final case class Extractor[S,T](read:PFunction[S,T]) {
	def unapply(s:S):Option[T]	= read(s)
	
	/** symbolic alias for andThen */
	def >=>[U](that:Extractor[T,U]):Extractor[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Extractor[R,S]):Extractor[R,T]	=
			this compose that
		
	def compose[R](that:Extractor[R,S]):Extractor[R,T]	=
			that andThen this
			
	def andThen[U](that:Extractor[T,U]):Extractor[S,U]	=
			Extractor(s => this read s flatMap that.read)
	
	def orElse(that:Extractor[S,T]):Extractor[S,T]	=
			Extractor(s	=> (this read s) orElse (that read s)) 
			
	def map[U](func:T=>U):Extractor[S,U]	=
			Extractor(s => read(s) map func)
		
	def contraMap[R](func:R=>S):Extractor[R,T]	=
			Extractor(func andThen read)
		
	def filter(pred:T=>Boolean):Extractor[S,T]	= 
			Extractor(s	=> read(s) filter pred)
		
	def cofilter(pred:S=>Boolean):Extractor[S,T]	= 
			Extractor(s	=> if (pred(s))	read(s)	else None)
			
	def asPFunction:PFunction[S,T]	= 
			s => read(s)
	
	def asPartialFunction:PartialFunction[S,T]	= 
			Function unlift read	
}
