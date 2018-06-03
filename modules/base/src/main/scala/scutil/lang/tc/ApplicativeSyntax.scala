package scutil.lang.tc

object ApplicativeSyntax extends ApplicativeSyntax

trait ApplicativeSyntax {
	implicit class ApplicativePureSyntaxExt[T](peer:T) {
		def pure[F[_]](implicit MF:Applicative[F]):F[T]	= MF pure peer
	}
	
	implicit class ApplicativeValueSyntaxExt[F[_],T](peer:F[T])(implicit MF:Applicative[F]) {
		def pa[U](func:F[T=>U]):F[U]					= (MF ap peer)(func)
		def combine[U,V](that:F[U])(func:(T,U)=>V):F[V]	= (MF combine (peer, that))(func)
	}
	
	/*
	implicit class ApplicativeArrowSyntaxExt[F[_],S,T](peer:F[S=>T])(implicit MF:Applicative[F]) {
		// TODO this collides with the same method defined in e.g. EitherExt
		def ap(it:F[S]):F[T]	= (MF ap it)(peer)
		def aping:F[S]=>F[T]	= pa _
	}
	*/
}
