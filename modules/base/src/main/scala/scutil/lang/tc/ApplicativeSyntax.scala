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
		@deprecated("use map2", "0.181.0")
		def combine[U,V](that:F[U])(func:(T,U)=>V):F[V]	= F.map2(peer, that)(func)
		def map2[U,V](that:F[U])(func:(T,U)=>V):F[V]	= F.map2(peer, that)(func)

		def tuple[U](that:F[U]):F[(T,U)]	= F.tuple(peer, that)
		def first[U](that:F[U]):F[T]		= F.first(peer, that)
		def second[U](that:F[U]):F[U]		= F.second(peer, that)

		def <* [U](that:F[U]):F[T]	= first(that)
		def *> [U](that:F[U]):F[U]	= second(that)
	}

	implicit final class ApplicativeArrowSyntaxExt[F[_],S,T](peer:F[S=>T])(implicit F:Applicative[F]) {
		// TODO as ap, this collides with the same method defined in e.g. EitherExt
		def apx(it:F[S]):F[T]	= F.ap(it)(peer)
		def aping:F[S]=>F[T]	= apx _
	}
}
