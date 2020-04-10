package scutil.lang.tc

object FunctorSyntax extends FunctorSyntax

trait FunctorSyntax {
	implicit final class FunctorValueSyntaxExt[F[_],T](peer:F[T])(implicit FC:Functor[F]) {
		def map[U](func:T=>U):F[U]	= FC.map(peer)(func)
		def void:F[Unit]			= FC.map(peer)(_ => ())
		def as[U](value:U):F[U]		= FC.map(peer)(_ => value)
	}

	implicit final class FunctorArrowSyntaxExt[F[_],S,T](peer:S=>T)(implicit FC:Functor[F]) {
		// aka liftF
		val mapping:F[S]=>F[T]	= fs => FC.map(fs)(peer)
	}
}
