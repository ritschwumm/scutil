package scutil.lang

import scala.util.control.Exception._

object Bijection {
	def identity[T]:Bijection[T,T]	= 
			Bijection(Predef.identity, Predef.identity)
}

final case class Bijection[S,T](write:S=>T, read:T=>S) {
	def inverse:Bijection[T,S]	= 
			Bijection(read, write)
	
	/** symbolic alias for andThen */
	@inline
	def >=>[U](that:Bijection[T,U]):Bijection[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	@inline
	def <=<[R](that:Bijection[R,S]):Bijection[R,T]	= 
			this compose that
		
	def compose[R](that:Bijection[R,S]):Bijection[R,T]	= 
			that andThen this
	
	def andThen[U](that:Bijection[T,U]):Bijection[S,U]	=
			Bijection(
					s	=> that write (this write s),
					u	=> this read  (that read  u))
			
	def asMarshaller:Marshaller[S,T]	= 
			Marshaller total (write, read)
		
	def asPBijection:PBijection[S,T]	=
			PBijection(
					it => Some(write(it)), 
					it => Some(read(it)))
}
