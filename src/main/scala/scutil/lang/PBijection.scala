package scutil.lang

object PBijection {
	def apply[S,T](writeFunc:PFunction[S,T], readFunc:PFunction[T,S]):PBijection[S,T]	= 
			new FunctionPBijection[S,T](writeFunc, readFunc)
			
	def partial[S,T](writeFunc:PartialFunction[S,T], readFunc:PartialFunction[T,S]):PBijection[S,T]	= 
			new FunctionPBijection[S,T](writeFunc.lift, readFunc.lift)
			
	def identity[T]:PBijection[T,T]	= 
			PBijection(Some.apply, Some.apply)
			
	def trivial[T]:PBijection[T,T]	=
			PBijection(constant(None), constant(None))
}

/** a partial Bijection */
trait PBijection[S,T] { self =>
	def write(s:S):Option[T]
	def read(t:T):Option[S]
	
	final def inverse:PBijection[T,S]	=
			PBijection(read, write)
	
	final def compose[R](that:PBijection[R,S]):PBijection[R,T]	= 
			that andThen this
	
	final def andThen[U](that:PBijection[T,U]):PBijection[S,U]	=
			PBijection(
					s	=> this write	s flatMap that.write,
					u	=> that read	u flatMap this.read)
			
	final def asBijection:Bijection[Option[S],Option[T]]	=
			Bijection(_ flatMap write, _ flatMap read)
			
	final def readExtractor:Extractor[T,S]	= Extractor(read)
	final def writeExtractor:Extractor[S,T]	= Extractor(write)
}

private final class FunctionPBijection[S,T](writeFunc:PFunction[S,T], readFunc:PFunction[T,S]) extends PBijection[S,T] {
	def write(s:S):Option[T]	= writeFunc(s)
	def read(t:T):Option[S]		= readFunc(t)
}
