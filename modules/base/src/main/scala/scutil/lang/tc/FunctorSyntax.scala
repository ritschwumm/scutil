package scutil.lang.tc

object FunctorSyntax extends FunctorSyntax

trait FunctorSyntax {
	implicit final class FunctorValueSyntaxExt[F[_],T](peer:F[T])(implicit FC:Functor[F]) {
		def map[U](func:T=>U):F[U]			= FC.map(peer)(func)
		def void:F[Unit]					= FC.void(peer)
		def as[U](value:U):F[U]				= FC.as(peer)(value)
		def zipBy[U](func:T=>U):F[(T,U)]	= FC.zipBy(peer)(func)
	}

	implicit final class FunctorArrowSyntaxExt[F[_],S,T](peer:S=>T)(implicit FC:Functor[F]) {
		// aka liftF
		val mapping:F[S]=>F[T]	= FC.mapping(peer)
	}
}
