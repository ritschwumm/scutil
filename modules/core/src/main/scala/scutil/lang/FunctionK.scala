package scutil.lang

object FunctionK {
	def identity[F[_]]:F ~> F	= [A] => (it:F[A]) => it

	extension [F[_], G[_]](self:F ~> G) {
		def composeK[E[_]](that:E ~> F):E ~> G	=
			that.andThenK(self)

		def andThenK[H[_]](that:G ~> H):F ~> H	=
			[A] => (a) => that(self(a))
	}
}

// aka FunctionK
type FunctionK[F[_], G[_]]	= [A] => F[A] => G[A]

/*
NOTE lambda conversion from [A] => F[A] => G[A] to this trait does not work
@see https://docs.scala-lang.org/sips/polymorphic-eta-expansion.html

trait FunctionK[-F[_],+G[_]] { self =>
	def apply[A](fa:F[A]):G[A]

	def compose[E[_]](that:E ~> F):E ~> G =
		that.andThen(this)

	def andThen[H[_]](that:G ~> H):F ~> H	=
		// TODO dotty should work, but does not @see https://docs.scala-lang.org/sips/polymorphic-eta-expansion.html
		// [A] => (it:F[A]) => that(self(it))
		new FunctionK[F, H] {
			def apply[A](it:F[A]):H[A]	=
				that(self(it))
		}
}
*/
