package scutil

// TODO everything having apply and unapply should implicitly be a Marshaller

object Marshaller {
	def zero[T]:Marshaller[T,T] = marshaller[T,T](identity, Some(_))
	
	def marshaller[S,T](applyFunc:S=>T, unapplyFunc:T=>Option[S]):Marshaller[S,T] = 
			new FunctionMarshaller[S,T](applyFunc, unapplyFunc)
			
	def marshallerPartial[S,T](applyFunc:S=>T, unapplyFunc:PartialFunction[T,S]):Marshaller[S,T] = 
			new FunctionMarshaller[S,T](applyFunc, unapplyFunc.lift)
	
	def marshallerUnpartial[S,T](applyFunc:S=>T, unapplyFunc:T=>S):Marshaller[S,T] = 
			new FunctionMarshaller[S,T](applyFunc, unapplyFunc andThen Some.apply)
	
	def doMarshall[S,T](s:S)(implicit marshaller:Marshaller[S,T]):T				= marshaller apply		s
	def doUnmarshall[S,T](t:T)(implicit marshaller:Marshaller[S,T]):Option[S]	= marshaller unapply	t
}

/** parser and unparser for some data into a side format */
trait Marshaller[S,T] {
	def apply(s:S):T
	def unapply(t:T):Option[S]
	
	final def compose[R](that:Marshaller[R,S]):Marshaller[R,T]	=
			that andThen this
			
	final def andThen[U](that:Marshaller[T,U]):Marshaller[S,U]	= new FunctionMarshaller[S,U](
			s	=> that apply (this apply s),
			u	=> for { t <- that unapply u; s <- this unapply t } yield s)
					
	final def orElse(that:Marshaller[S,T]):Marshaller[S,T]	= new FunctionMarshaller[S,T](
			s	=> this apply s,
			t	=> (this unapply t) orElse (that unapply t))
}

private final class FunctionMarshaller[S,T](applyFunc:S=>T, unapplyFunc:T=>Option[S]) extends Marshaller[S,T] {
	def apply(s:S):T			= applyFunc(s)
	def unapply(t:T):Option[S]	= unapplyFunc(t)
}
