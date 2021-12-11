package scutil.lang.tc

object FunctorSyntax extends FunctorSyntax

trait FunctorSyntax {
	implicit final class FunctorValueSyntaxExt[F[_],T](peer:F[T])(using FC:Functor[F]) {
		def map[U](func:T=>U):F[U]			= FC.map(peer)(func)
		def void:F[Unit]					= FC.void(peer)
		def as[U](value:U):F[U]				= FC.as(peer)(value)
		def fproduct[U](func:T=>U):F[(T,U)]	= FC.fproduct(peer)(func)
	}

	implicit final class FunctorArrowSyntaxExt[F[_],S,T](peer:S=>T)(using FC:Functor[F]) {
		// TODO lift, or liftF?
		val lift:F[S]=>F[T]	= FC.lift(peer)
	}
}
