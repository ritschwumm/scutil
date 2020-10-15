package scutil.lang.tc

object ApplicativeSyntax extends ApplicativeSyntax

trait ApplicativeSyntax {
	implicit final class ApplicativePureSyntaxExt[T](peer:T) {
		def pure[F[_]](implicit F:Applicative[F]):F[T]	= F pure peer
	}

	implicit final class ApplicativeBooleanSyntax(peer:Boolean) {
		def ifA[F[_]](action:F[Unit])(implicit F:Applicative[F]):F[Unit]	= F.ifA(peer)(action)
	}

	implicit final class ApplicativeOptionSyntax[T](peer:Option[T]) {
		def optionalA[F[_]](action:T=>F[Unit])(implicit F:Applicative[F]):F[Unit]	= F.optionalA(peer)(action)
	}

	implicit final class ApplicativeValueSyntaxExt[F[_],T](peer:F[T])(implicit F:Applicative[F]) {
		def pa[U](func:F[T=>U]):F[U]					= F.ap(peer)(func)
		def combine[U,V](that:F[U])(func:(T,U)=>V):F[V]	= F.combine(peer, that)(func)
	}

	/*
	implicit final class ApplicativeArrowSyntaxExt[F[_],S,T](peer:F[S=>T])(implicit F:Applicative[F]) {
		// NOTE this collides with the same method defined in e.g. EitherExt
		def ap(it:F[S]):F[T]	= F.ap(it)(peer)
		def aping:F[S]=>F[T]	= pa _
	}
	*/
}
