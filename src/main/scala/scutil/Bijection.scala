package scutil

import scala.util.control.Exception._

object Bijection {
	def apply[S,T](writeFunc:S=>T, readFunc:T=>S):Bijection[S,T]	= 
			new FunctionBijection[S,T](writeFunc,readFunc)
		
	def identity[T]:Bijection[T,T]	= Bijection(
			Predef.identity,
			Predef.identity)
		
	/** attention: throws exceptions when not matching. do not use this unless you know what you are doing */
	def marshallerFailing[S,T](applyFunc:S=>T, unapplyFunc:T=>Option[S]):Bijection[S,T]	= Bijection[S,T](
			applyFunc,
			it => unapplyFunc(it) getOrElse (sys error ("cannot unmarshall: " + it)))
}

trait Bijection[S,T] {
	/*
	// can be used as function and extractor
	final def apply(s:S):T				= write(s)
	final def unapply(t:T):Option[S]	= Some(read(t))
	*/
	
	def write(s:S):T
	def read(t:T):S
	
	final def inverse:Bijection[T,S]	= Bijection(
			read, 
			write)
	
	final def compose[R](that:Bijection[R,S]):Bijection[R,T]	= 
			that andThen this
	
	final def andThen[U](that:Bijection[T,U]):Bijection[S,U]	= Bijection(
			s	=> that write (this write s),
			u	=> this read  (that read  u))
			
	final def asMarshaller:Marshaller[S,T]	= 
			Marshaller unpartial (write, read)
		
	final def asMarshallerCatchingRead:Marshaller[S,T]	= 
			Marshaller(write, it => allCatch opt read(it))
		
	final def asPartialBijection:PartialBijection[S,T]	= PartialBijection(
			it => Some(write(it)), 
			it => Some(read(it)))
			
	final def readFunction:Function1[T,S]	= read _
	final def writeFunction:Function1[S,T]	= write _
	
	/*
	// TODO generalize to any Functor
	def liftSeq:Bijection[Seq[S],Seq[T]]	= Bijection(
			_ map write,
			_ map read)
	*/
}

private final class FunctionBijection[S,T](writeFunc:S=>T, readFunc:T=>S) extends Bijection[S,T] {
	def write(s:S):T	= writeFunc(s)
	def read(t:T):S		= readFunc(t)
}
