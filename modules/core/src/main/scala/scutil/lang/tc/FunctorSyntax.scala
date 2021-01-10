package scutil.lang.tc

object FunctorSyntax extends FunctorSyntax

trait FunctorSyntax {
	implicit final class FunctorValueSyntaxExt[F[_],T](peer:F[T])(implicit FC:Functor[F]) {
		def map[U](func:T=>U):F[U]			= FC.map(peer)(func)
		def void:F[Unit]					= FC.void(peer)
		def as[U](value:U):F[U]				= FC.as(peer)(value)
		@deprecated("use fproduct", "0.195.0")
		def zipBy[U](func:T=>U):F[(T,U)]	= FC.zipBy(peer)(func)
		def fproduct[U](func:T=>U):F[(T,U)]	= FC.fproduct(peer)(func)
	}

	implicit final class FunctorArrowSyntaxExt[F[_],S,T](peer:S=>T)(implicit FC:Functor[F]) {
		// TODO lift, or liftF?
		val lift:F[S]=>F[T]	= FC.lift(peer)

		@deprecated("use lift", "0.195.0")
		val mapping:F[S]=>F[T]	= lift
	}
}
