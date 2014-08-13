package scutil.lang

object Bijection {
	def identity[T]:Bijection[T,T]	= 
			Bijection(Predef.identity, Predef.identity)
}

final case class Bijection[S,T](write:S=>T, read:T=>S) {
	def lift(func:Endo[T]):Endo[S]	=
			write andThen func andThen read
		
	def inverse:Bijection[T,S]	= 
			Bijection(read, write)
	
	/** symbolic alias for andThen */
	def >=>[U](that:Bijection[T,U]):Bijection[S,U]	=
			this andThen that
		
	/** symbolic alias for compose */
	def <=<[R](that:Bijection[R,S]):Bijection[R,T]	= 
			this compose that
		
	def compose[R](that:Bijection[R,S]):Bijection[R,T]	= 
			that andThen this
	
	def andThen[U](that:Bijection[T,U]):Bijection[S,U]	=
			Bijection(
				s	=> that write (this write s),
				u	=> this read  (that read  u)
			)
					
	def andThenPrism[U](that:Prism[T,U]):Prism[S,U]	=
			asPrism >=> that
					
	def andThenPBijection[U](that:PBijection[T,U]):PBijection[S,U]	=
			asPBijection >=> that
					
	def andThenTLens[U](that:TLens[T,U]):TLens[S,U]	=
			asTLens >=> that
			
	def andThenPLens[U](that:PLens[T,U]):PLens[S,U]	=
			asPLens >=> that
		
	def asPrism:Prism[S,T]	= 
			Prism total (write, read)
		
	def asPBijection:PBijection[S,T]	=
			PBijection total (write, read)
					
	def asTLens:TLens[S,T]	=
			TLens { s	=>
				Store(write(s), read)
			}
					
	def asPLens:PLens[S,T]	=
			PLens { s	=>
				Some(Store(write(s), read))
			}
}
