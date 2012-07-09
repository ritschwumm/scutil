package scutil.data

object PartialBijection {
	def apply[S,T](writeFunc:S=>Option[T], readFunc:T=>Option[S]):PartialBijection[S,T]	= 
			new FunctionPartialBijection[S,T](writeFunc, readFunc)
			
	def partial[S,T](writeFunc:PartialFunction[S,T], readFunc:PartialFunction[T,S]):PartialBijection[S,T]	= 
			new FunctionPartialBijection[S,T](writeFunc.lift, readFunc.lift)
			
	def identity[T]:PartialBijection[T,T]	= PartialBijection(
			Some.apply,
			Some.apply)
			
	def trivial[T]:PartialBijection[T,T]	= PartialBijection(
			_ => None,
			_ => None)
}

trait PartialBijection[S,T] { self =>
	def write(s:S):Option[T]
	def read(t:T):Option[S]
	
	final def inverse:PartialBijection[T,S]	= PartialBijection(
			read, 
			write)
	
	final def compose[R](that:PartialBijection[R,S]):PartialBijection[R,T]	= 
			that andThen this
	
	final def andThen[U](that:PartialBijection[T,U]):PartialBijection[S,U]	= PartialBijection(
			s	=> this write	s flatMap that.write,
			u	=> that read	u flatMap this.read)
			
	final def asBijection:Bijection[Option[S],Option[T]]	= Bijection(
			_ flatMap write, 
			_ flatMap read)
			
	final def readExtractor:Extractor[T,S]	= Extractor(read _)
	final def writeExtractor:Extractor[S,T]	= Extractor(write _)
}

private final class FunctionPartialBijection[S,T](writeFunc:S=>Option[T], readFunc:T=>Option[S]) extends PartialBijection[S,T] {
	def write(s:S):Option[T]	= writeFunc(s)
	def read(t:T):Option[S]		= readFunc(t)
}
