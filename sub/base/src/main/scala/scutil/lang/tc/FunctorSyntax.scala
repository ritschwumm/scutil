package scutil.lang.tc

object FunctorSyntax extends FunctorSyntax

trait FunctorSyntax {
	implicit class FunctorValueSyntaxExt[F[_],T](peer:F[T])(implicit FC:Functor[F]) {
		def map[U](func:T=>U):F[U]	= FC.map(peer)(func)
	}
	
	implicit class FunctorArrowSyntax[F[_],S,T](peer:S=>T)(implicit FC:Functor[F]) {
		// aka liftF
		val mapping:F[S]=>F[T]	= fs => FC.map(fs)(peer)
	}
}