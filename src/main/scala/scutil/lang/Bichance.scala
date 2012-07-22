package scutil.lang

object Bichance {
	def apply[S,T](writeFunc:S=>Option[T], readFunc:T=>Option[S]):Bichance[S,T]	= 
			new FunctionBichance[S,T](writeFunc, readFunc)
			
	def partial[S,T](writeFunc:PartialFunction[S,T], readFunc:PartialFunction[T,S]):Bichance[S,T]	= 
			new FunctionBichance[S,T](writeFunc.lift, readFunc.lift)
			
	def identity[T]:Bichance[T,T]	= Bichance(
			Some.apply,
			Some.apply)
			
	def trivial[T]:Bichance[T,T]		= Bichance(
			_ => None,
			_ => None)
}

/** a partial Bijection */
trait Bichance[S,T] { self =>
	def write(s:S):Option[T]
	def read(t:T):Option[S]
	
	final def inverse:Bichance[T,S]	= Bichance(
			read, 
			write)
	
	final def compose[R](that:Bichance[R,S]):Bichance[R,T]	= 
			that andThen this
	
	final def andThen[U](that:Bichance[T,U]):Bichance[S,U]	= Bichance(
			s	=> this write	s flatMap that.write,
			u	=> that read	u flatMap this.read)
			
	final def asBijection:Bijection[Option[S],Option[T]]	= Bijection(
			_ flatMap write, 
			_ flatMap read)
			
	final def readExtractor:Extractor[T,S]	= Extractor(read _)
	final def writeExtractor:Extractor[S,T]	= Extractor(write _)
}

private final class FunctionBichance[S,T](writeFunc:S=>Option[T], readFunc:T=>Option[S]) extends Bichance[S,T] {
	def write(s:S):Option[T]	= writeFunc(s)
	def read(t:T):Option[S]		= readFunc(t)
}
