package scutil.lang.tc

object MonadSyntax {
	extension [F[_]:Monad as MF,T](peer:F[T]) {
		def flatMap[U](func:T=>F[U]):F[U]	= MF.flatMap(peer)(func)
	}

	extension [F[_]:Monad as MF,T](peer:F[F[T]]) {
		def flatten:F[T]	= MF.flatten(peer)
	}

	extension [F[_]:Monad as MF,S,T](peer:S=>F[T]) {
		// aka liftM
		def flatMapping:F[S]=>F[T]	= MF.flatMapping(peer)
	}
}
