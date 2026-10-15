package scutil.lang.tc

object FunctorSyntax {
	extension [F[_],T](peer:F[T])(using FC:Functor[F]) {
		def map[U](func:T=>U):F[U]			= FC.map(peer)(func)
		def void:F[Unit]					= FC.void(peer)
		def as[U](value:U):F[U]				= FC.as(peer)(value)
		def fproduct[U](func:T=>U):F[(T,U)]	= FC.fproduct(peer)(func)
	}

	extension [F[_],S,T](peer:S=>T)(using FC:Functor[F]) {
		// TODO lift, or liftF?
		def lift:F[S]=>F[T]	= FC.lift(peer)
	}
}
