package scutil.lang

object Extractor {
	def apply[S,T](func:PFunction[S,T]):Extractor[S,T]	= 
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
	
	final def compose[R](that:Extractor[R,S]):Extractor[R,T]	=
			that andThen this
			
	final def andThen[U](that:Extractor[T,U]):Extractor[S,U]	= Extractor(
			s => this read s flatMap that.read)
	
	final def orElse(that:Extractor[S,T]):Extractor[S,T]	= Extractor(
			s	=> (this read s) orElse (that read s)) 
			
	// TODO check method names
	
	final def map[U](func:T=>U):Extractor[S,U]	=
			Extractor(s => read(s) map func)
		
	final def contraMap[R](func:R=>S):Extractor[R,T]	=
			Extractor(func andThen read)
		
	final def filter(pred:T=>Boolean):Extractor[S,T]	= 
			Extractor(s	=> read(s) filter pred)
		
	final def cofilter(pred:S=>Boolean):Extractor[S,T]	= 
			Extractor(s	=> if (pred(s))	read(s)	else None)
			
	final def asFunction:PFunction[S,T]	= 
			s => read(s)
	
	final def asPartialFunction:PartialFunction[S,T]	= 
			Function unlift read	
}

private final class FunctionExtractor[S,T](readFunc:PFunction[S,T]) extends Extractor[S,T] {
	def read(s:S):Option[T]	= readFunc(s)
}
