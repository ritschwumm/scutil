package scutil

object Marshaller {
	def apply[S,T](writeFunc:S=>T, readFunc:T=>Option[S]):Marshaller[S,T] = 
			new FunctionMarshaller[S,T](writeFunc, readFunc)
			
	def partial[S,T](writeFunc:S=>T, readFunc:PartialFunction[T,S]):Marshaller[S,T] = 
			Marshaller(writeFunc, readFunc.lift)
	
	def unpartial[S,T](writeFunc:S=>T, readFunc:T=>S):Marshaller[S,T] = 
			Marshaller(writeFunc, readFunc andThen Some.apply)
			
	def identity[T]:Marshaller[T,T] = 
			unpartial[T,T](Predef.identity[T], Predef.identity[T])
			
	def guarded[T](predicate:T=>Boolean):Marshaller[T,T]	= Marshaller(
			Predef.identity, 
			it => if (predicate(it)) Some(it) else None)
	
	/*
	// NOTE isn't this a Marshaller, too?
	def marshall[S,T](s:S)(implicit marshaller:Marshaller[S,T]):T			= marshaller apply		s
	def unmarshall[S,T](t:T)(implicit marshaller:Marshaller[S,T]):Option[S]	= marshaller unapply	t
	*/
}

/** parser and unparser for some data into a side format */
trait Marshaller[S,T] {
	// can be used as scala function and extractor
	final def apply(s:S):T				= write(s)
	final def unapply(t:T):Option[S]	= read(t)
	
	def write(s:S):T
	def read(t:T):Option[S]
	
	final def compose[R](that:Marshaller[R,S]):Marshaller[R,T]	=
			that andThen this
			
	final def andThen[U](that:Marshaller[T,U]):Marshaller[S,U]	= Marshaller(
			s	=> that write (this write s),
			u	=> that read u flatMap this.read)
					
	final def orElse(that:Marshaller[S,T]):Marshaller[S,T]	= Marshaller(
			s	=> this write s,
			t	=> (this read t) orElse (that read t))
			
	/** map the target value in both directions */
	final def xmapForward[U](bijection:Bijection[T,U]):Marshaller[S,U]	= Marshaller(
			s	=> bijection write (this write s),
			u	=> this read (bijection read u))
			
	/** map the source value in both directions */
	final def xmapReverse[R](bijection:Bijection[R,S]):Marshaller[R,T]	= Marshaller(
			r	=> this write (bijection write r),
			u	=> this read u map bijection.read)
		
	final def cofilterBefore(pred:T=>Boolean):Marshaller[S,T]	= Marshaller(
			s	=> write(s),
			t	=> if (pred(t)) read(t) else None)
			
	final def cofilterAfter(pred:S=>Boolean):Marshaller[S,T]	= Marshaller(
			s	=> write(s),
			t	=> read(t) filter pred)
			
	final def asPartialBijection:PartialBijection[S,T]	= PartialBijection(
			it => Some(write(it)), 
			read)
			
	final def asBijection(default: =>S):Bijection[S,T]	= Bijection[S,T](
			it => write(it),
			it => read(it) getOrElse default)
			
	/** attention: throws exceptions when not matching. do not use this unless you know what you are doing */
	final def asBijectionFailing:Bijection[S,T]	= Bijection[S,T](
			it => write(it),
			it => read(it) getOrElse (sys error ("cannot unmarshall: " + it)))
			
	final def readExtractor:Extractor[T,S]	= Extractor(read _)
	final def writeFunction:Function1[S,T]	= write _
}

private final class FunctionMarshaller[S,T](writeFunc:S=>T, readFunc:T=>Option[S]) extends Marshaller[S,T] {
	def write(s:S):T		= writeFunc(s)
	def read(t:T):Option[S]	= readFunc(t)
}
