package scutil.lang.tc

object MonadSyntax extends MonadSyntax

trait MonadSyntax {
	implicit class MonadPureSyntaxExt[T](peer:T) {
		def pure[F[_]](implicit MF:Monad[F]):F[T]	= MF pure peer
	}
	
	implicit class MonadBindSyntaxExt[F[_],T](peer:F[T])(implicit MF:Monad[F]) {
		// we get this from FunctorSyntax
		//def map[U](func:T=>U):F[U]			= (MF map peer)(func)
		
		def flatMap[U](func:T=>F[U]):F[U]	= (MF flatMap peer)(func)
		def flatten[U](implicit func:T=>F[U]):F[U]	= flatMap(func)
	}
	
	implicit class MonadLiftSyntax[F[_],S,T](peer:S=>F[T])(implicit MF:Monad[F]) {
		// aka liftM
		val flatMapping:F[S]=>F[T]	= fs => MF.flatMap(fs)(peer)
	}
}
