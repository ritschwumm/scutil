package scutil.lang.tc

object MonadSyntax {
	extension [F[_],T](peer:F[T])(using MF:Monad[F]) {
		def flatMap[U](func:T=>F[U]):F[U]	= MF.flatMap(peer)(func)
	}

	extension [F[_],T](peer:F[F[T]])(using MF:Monad[F]) {
		def flatten:F[T]	= MF.flatten(peer)
	}

	extension [F[_],S,T](peer:S=>F[T])(using MF:Monad[F]) {
		// aka liftM
		def flatMapping:F[S]=>F[T]	= MF.flatMapping(peer)
	}
}
