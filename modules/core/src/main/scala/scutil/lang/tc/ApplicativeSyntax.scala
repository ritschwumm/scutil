package scutil.lang.tc

object ApplicativeSyntax extends ApplicativeSyntax

trait ApplicativeSyntax {
	implicit final class ApplicativePureSyntaxExt[T](peer:T) {
		def pure[F[_]](implicit F:Applicative[F]):F[T]	= F pure peer
	}

	implicit final class ApplicativeBooleanSyntax(peer:Boolean) {
		@deprecated("use whenA", "0.195.0")
		def ifA[F[_]](action:F[Unit])(implicit F:Applicative[F]):F[Unit]	= whenA(action)

		def whenA[F[_]](action: =>F[Unit])(implicit F:Applicative[F]):F[Unit]	= F.whenA(peer)(action)
		def unlessA[F[_]](action: =>F[Unit])(implicit F:Applicative[F]):F[Unit]	= F.unlessA(peer)(action)
	}

	implicit final class ApplicativeOptionSyntax[T](peer:Option[T]) {
		def optionalA[F[_]](action:T=>F[Unit])(implicit F:Applicative[F]):F[Unit]	= F.optionalA(peer)(action)
	}

	implicit final class ApplicativeValueSyntaxExt[F[_],T](peer:F[T])(implicit F:Applicative[F]) {
		def map2[U,V](that:F[U])(func:(T,U)=>V):F[V]	= F.map2(peer, that)(func)

		@deprecated("use product", "0.195.0")
		def tuple[U](that:F[U]):F[(T,U)]	= product(that)

		def product[U](that:F[U]):F[(T,U)]	= F.product(peer, that)

		@deprecated("use productL", "0.195.0")
		def first[U](that:F[U]):F[T]		= productL(that)

		def productL[U](that:F[U]):F[T]		= F.productL(peer, that)

		@deprecated("use productR", "0.195.0")
		def second[U](that:F[U]):F[U]		= productR(that)

		def productR[U](that:F[U]):F[U]		= F.productR(peer, that)

		def <* [U](that:F[U]):F[T]	= productL(that)
		def *> [U](that:F[U]):F[U]	= productR(that)
	}

	implicit final class ApplicativeArrowSyntaxExt[F[_],S,T](peer:F[S=>T])(implicit F:Applicative[F]) {
		def ap(it:F[S]):F[T]	= F.ap(peer)(it)
	}
}
