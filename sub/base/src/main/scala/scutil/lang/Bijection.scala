package scutil.lang

object Bijection {
	def identity[T]:Bijection[T,T]	=
			Bijection(Predef.identity, Predef.identity)
}

final case class Bijection[S,T](write:S=>T, read:T=>S) {
	def modify(s:S, func:Endo[T]):S		= read(func(write(s)))
	def modifier(func:Endo[T]):Endo[S]	= modify(_, func)
		
	def modifyOpt(s:S, func:PEndo[T]):Option[S]	= func(write(s)) map read
	def modifierOpt(func:PEndo[T]):PEndo[S]		= modifyOpt(_, func)
	
	def modifyStateful[X](s:S, func:Stateful[T,X]):(S,X)	= {
		val (t, x)	= func(write(s))
		(read(t), x)
	}
	def modifierStateful[X](func:Stateful[T,X]):Stateful[S,X]	= modifyStateful(_, func)
		
	//------------------------------------------------------------------------------
	
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
			toPrism >=> that
					
	def andThenPBijection[U](that:PBijection[T,U]):PBijection[S,U]	=
			toPBijection >=> that
					
	def andThenTLens[U](that:TLens[T,U]):TLens[S,U]	=
			toTLens >=> that
			
	def andThenPLens[U](that:PLens[T,U]):PLens[S,U]	=
			toPLens >=> that
		
	def toPrism:Prism[S,T]	=
			Prism total (write, read)
		
	def toPBijection:PBijection[S,T]	=
			PBijection total (write, read)
					
	def toTLens:TLens[S,T]	=
			TLens { s	=>
				Store(write(s), read)
			}
					
	def toPLens:PLens[S,T]	=
			PLens { s	=>
				Some(Store(write(s), read))
			}
}
