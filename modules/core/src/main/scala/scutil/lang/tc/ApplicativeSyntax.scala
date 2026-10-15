package scutil.lang.tc

object ApplicativeSyntax {
	extension [T](peer:T) {
		def pure[F[_]:Applicative as F]:F[T]	= F.pure(peer)
	}

	extension (peer:Boolean) {
		def whenA[F[_]:Applicative as F](action: =>F[Unit]):F[Unit]		= F.whenA(peer)(action)
		def unlessA[F[_]:Applicative as F](action: =>F[Unit]):F[Unit]	= F.unlessA(peer)(action)
	}

	extension [T](peer:Option[T]) {
		def optionalA[F[_]:Applicative as F](action:T=>F[Unit]):F[Unit]	= F.optionalA(peer)(action)
	}

	extension [F[_]:Applicative as F,T](peer:F[T]) {
		def map2[U,V](that:F[U])(func:(T,U)=>V):F[V]	= F.map2(peer, that)(func)

		def product[U](that:F[U]):F[(T,U)]	= F.product(peer, that)

		def productL[U](that:F[U]):F[T]		= F.productL(peer, that)
		def productR[U](that:F[U]):F[U]		= F.productR(peer, that)

		def <* [U](that:F[U]):F[T]	= productL(that)
		def *> [U](that:F[U]):F[U]	= productR(that)
	}

	extension [F[_]:Applicative as F,S,T](peer:F[S=>T]) {
		def ap(it:F[S]):F[T]	= F.ap(peer)(it)
	}
}
