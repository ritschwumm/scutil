package scutil

object Extractor {
	def total[S,T](func:S=>T):Extractor[S,T]	= new Extractor[S,T] {
		def unapply(s:S):Option[T]	= Some(func(s))
	}
	
	def partial[S,T](func:PartialFunction[S,T]):Extractor[S,T]	= new Extractor[S,T] {
		def unapply(s:S):Option[T]	= if (func isDefinedAt s) Some(func apply s) else None
	}
	
	def optional[S,T](func:Function[S,Option[T]]):Extractor[S,T]	= new Extractor[S,T] {
		def unapply(s:S):Option[T]	= func(s)
	}
	
	/*
	def structural[S,T](func:{ def unapply(s:S):Option[T] })(implicit ms:Manifest[S], mt:Manifest[T]):Extractor[S,T]	= new Extractor[S,T] {
		def unapply(s:S):Option[T]	= func unapply s
	}
	*/
}
	
/** representative extractor (as opposed to compiler magic) */
trait Extractor[S,T] {
	def unapply(s:S):Option[T]
	
	def compose[R](that:Extractor[R,S]):Extractor[R,T]	=
			that andThen this
			
	def andThen[U](that:Extractor[T,U]):Extractor[S,U]	=
			Extractor optional { s => 
				//for { t <- this unapply s; u <- that unapply t } yield u
				this unapply s flatMap { that unapply _ }
			}
	
	def orElse(that:Extractor[S,T]):Extractor[S,T]	=
			Extractor optional { s => 
				(this unapply s) orElse (that unapply s) 
			}
			
	def toFunction:Function[S,Option[T]]	= new Function[S,Option[T]] {
		def apply(s:S):Option[T]	= unapply(s)
	}
	
	def toPartialFunction:PartialFunction[S,T]	= new PartialFunction[S,T] {
		def apply(s:S):T				= unapply(s).get
		def isDefinedAt(s:S):Boolean	= unapply(s).isDefined
	}
}
