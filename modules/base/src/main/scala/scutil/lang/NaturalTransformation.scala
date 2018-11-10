package scutil.lang

trait NaturalTransformation[-F[_],+G[_]] { self =>
	def apply[A](fa:F[A]):G[A]

	def compose[E[_]](that:E ~> F):E ~> G =
			that andThen this

	def andThen[H[_]](that:G ~> H):F ~> H	=
			new NaturalTransformation[F,H] {
				def apply[A](it:F[A]):H[A]	=
					that(self(it))
			}
}
