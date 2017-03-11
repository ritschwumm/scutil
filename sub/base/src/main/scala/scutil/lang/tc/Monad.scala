package scutil.lang.tc

object Monad {
	def apply[F[_]](implicit ev:Monad[F]):Monad[F]	= ev
}

trait Monad[F[_]] extends Functor[F] {
	def pure[T](it:T):F[T]
	def flatMap[S,T](its:F[S])(func:S=>F[T]):F[T]
	
	def map[S,T](its:F[S])(func:S=>T):F[T]	= flatMap(its)(func andThen pure[T])
	def flatten[T](its:F[F[T]]):F[T]		= flatMap(its)(identity)
}
