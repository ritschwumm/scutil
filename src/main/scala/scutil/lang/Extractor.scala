package scutil.lang

object Extractor {
	def apply[S,T](func:Chance[S,T]):Extractor[S,T]	= 
			new FunctionExtractor(func)
	
	def total[S,T](func:S=>T):Extractor[S,T]	= Extractor(
			s	=> Some(func(s)))
	
	def partial[S,T](func:PartialFunction[S,T]):Extractor[S,T]	= Extractor(
			s	=> if (func isDefinedAt s) Some(func apply s) else None)
	
	def guarding[T](pred:Predicate[T]):Extractor[T,T]	=
			Extractor(it => if (pred(it)) Some(it) else None)
			
	def identity[T]:Extractor[T,T]	= 
			Extractor(Some.apply)
			
	def trivial[T]:Extractor[T,Any]	= 
			Extractor(_ => None)
}
	
/** representative extractor (as opposed to compiler magic) */
trait Extractor[S,T] {
	final def unapply(s:S):Option[T]	= read(s)
	
	def read(s:S):Option[T]
	
	def compose[R](that:Extractor[R,S]):Extractor[R,T]	=
			that andThen this
			
	def andThen[U](that:Extractor[T,U]):Extractor[S,U]	= Extractor(
			s => this read s flatMap that.read)
	
	def orElse(that:Extractor[S,T]):Extractor[S,T]	= Extractor(
			s	=> (this read s) orElse (that read s)) 
			
	def asFunction:Function[S,Option[T]]	= 
			s => read(s)
	
	def asPartialFunction:PartialFunction[S,T]	= new PartialFunction[S,T] {
		def apply(s:S):T				= read(s).get
		def isDefinedAt(s:S):Boolean	= read(s).isDefined
	}
}

private final class FunctionExtractor[S,T](readFunc:S=>Option[T]) extends Extractor[S,T] {
	def read(s:S):Option[T]		= readFunc(s)
}
