package scutil

import scutil.structure.Functor

object Bijection {
	def apply[S,T](writeFunc:S=>T, readFunc:T=>S):Bijection[S,T]	= 
			new FunctionBijection[S,T](writeFunc,readFunc)
			
	def identity[T]:Bijection[T,T]	= Bijection(
			Predef.identity,
			Predef.identity)
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
			
	// TODO better name?
	final def lift[M[_]](functor:Functor[M]):Bijection[M[S],M[T]]	= Bijection(
			s	=> functor map (s, write),
			t	=> functor map (t, read))
	
	final def asMarshaller:Marshaller[S,T]	= 
			Marshaller unpartial (write, read)
			
	final def asPartialBijection:PartialBijection[S,T]	= PartialBijection(
			it => Some(write(it)), 
			it => Some(read(it)))
}

private final class FunctionBijection[S,T](writeFunc:S=>T, readFunc:T=>S) extends Bijection[S,T] {
	def write(s:S):T	= writeFunc(s)
	def read(t:T):S		= readFunc(t)
}
