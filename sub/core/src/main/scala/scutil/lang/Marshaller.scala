package scutil.lang

object Marshaller {
	def partial[S,T](writeFunc:S=>T, readFunc:PartialFunction[T,S]):Marshaller[S,T] = 
			Marshaller(writeFunc, readFunc.lift)
	
	def total[S,T](writeFunc:S=>T, readFunc:T=>S):Marshaller[S,T] = 
			Marshaller(writeFunc, readFunc andThen Some.apply)
			
	def identity[T]:Marshaller[T,T] = 
			total[T,T](Predef.identity[T], Predef.identity[T])
		
	def always[T]:Marshaller[T,Option[T]]	=
			Marshaller(Some.apply, Predef.identity)
			
	def guarded[T](predicate:T=>Boolean):Marshaller[T,T]	=
			Marshaller(
					Predef.identity, 
					it => if (predicate(it)) Some(it) else None)
}

/** parser and unparser for some data into a side format */
final case class Marshaller[S,T](write:S=>T, read:PFunction[T,S]) {
	// can be used as scala function and extractor
	def apply(s:S):T				= write(s)
	def unapply(t:T):Option[S]	= read(t)
	
	/** symbolic alias for andThen */
	def >=>[U](that:Marshaller[T,U]):Marshaller[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Marshaller[R,S]):Marshaller[R,T]	=
			this compose that
		
	def compose[R](that:Marshaller[R,S]):Marshaller[R,T]	=
			that andThen this
			
	def andThen[U](that:Marshaller[T,U]):Marshaller[S,U]	=
			Marshaller(
					s	=> that write (this write s),
					u	=> that read u flatMap this.read)
					
	def orElse(that:Marshaller[S,T]):Marshaller[S,T]	= 
			Marshaller(
					write,
					t	=> (this read t) orElse (that read t))
			
	/** map the source value in both directions, resembles compose */
	def xmapBefore[R](bijection:Bijection[R,S]):Marshaller[R,T]	=
			Marshaller(
					r	=> this write (bijection write r),
					u	=> this read u map bijection.read)
			
	/** map the target value in both directions, resembles andThen */
	def xmapAfter[U](bijection:Bijection[T,U]):Marshaller[S,U]	=
			Marshaller(
					s	=> bijection write (this write s),
					u	=> this read (bijection read u))
			
	/** filter the source value */
	def cofilterBefore(pred:Predicate[T]):Marshaller[S,T]	=
			Marshaller(
					write,
					t	=> if (pred(t)) read(t) else None)
			
	/** filter the target value */
	def cofilterAfter(pred:S=>Boolean):Marshaller[S,T]	=
			Marshaller(
					write,
					t	=> read(t) filter pred)
			
	def asPBijection:PBijection[S,T]	=
			PBijection(
					it => Some(write(it)), 
					read)
			
	def toBijection(func:T=>S):Bijection[S,T]	= 
			Bijection(
					write,
					it => read(it) getOrElse func(it))
					
	def toBijectionWith(default: =>S):Bijection[S,T]	= 
			toBijection(constant(default))
			
	def asPLens:PLens[T,S]	=
			PLens { this read _ map (Store(_, this.write)) }
			
	def readExtractor:Extractor[T,S]	=
			Extractor(read)
}
