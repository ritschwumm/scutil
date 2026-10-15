package scutil.lang

object NaturalTransformation {
	def identity[F[_]]:F ~> F	=
		// TODO dotty should work, but does not @see https://docs.scala-lang.org/sips/polymorphic-eta-expansion.html
		//[A] => (it:F[A]) => it
		new NaturalTransformation[F,F] {
			def apply[A](fa:F[A]):F[A]	= fa
		}
}

// TODO dotty this is just type NaturalTransformation[F[_], G[_]]	= [T] => F[T] => G[T]

// aka FunctionK
trait NaturalTransformation[-F[_],+G[_]] { self =>
	def apply[A](fa:F[A]):G[A]

	def compose[E[_]](that:E ~> F):E ~> G =
		that.andThen(this)

	def andThen[H[_]](that:G ~> H):F ~> H	=
		// TODO dotty should work, but does not @see https://docs.scala-lang.org/sips/polymorphic-eta-expansion.html
		// [A] => (it:F[A]) => that(self(it))
		new NaturalTransformation[F,H] {
			def apply[A](it:F[A]):H[A]	=
				that(self(it))
		}
}
