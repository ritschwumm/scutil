package scutil.lang.tc

object MonadSyntax extends MonadSyntax

trait MonadSyntax {
	implicit final class MonadValueSyntaxExt[F[_],T](peer:F[T])(implicit MF:Monad[F]) {
		def flatMap[U](func:T=>F[U]):F[U]	= MF.flatMap(peer)(func)
	}

	implicit final class MonadMonadValueSyntaxExt[F[_],T](peer:F[F[T]])(implicit MF:Monad[F]) {
		def flatten:F[T]	= MF.flatten(peer)
	}

	implicit final class MonadArrowSyntaxExt[F[_],S,T](peer:S=>F[T])(implicit MF:Monad[F]) {
		// aka liftM
		val flatMapping:F[S]=>F[T]	= MF.flatMapping(peer)
	}
}
