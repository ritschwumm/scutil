package scutil.structure

trait Functor[F[_]] {
	def map[S,T](v:F[S], f:S=>T):F[T]
}
