package scutil.lang.tc

object Monad {
	def apply[F[_]](using ev:Monad[F]):Monad[F]	= ev
}

trait Monad[F[_]] extends Applicative[F] {
	//------------------------------------------------------------------------------
	//## own

	def flatMap[S,T](its:F[S])(func:S=>F[T]):F[T]

	//------------------------------------------------------------------------------
	//## derived

	def flatten[T](its:F[F[T]]):F[T]	=
		flatMap(its)(identity)

	def flatMapping[S,T](func:S=>F[T]):F[S]=>F[T]	=
		flatMap(_)(func)

	//------------------------------------------------------------------------------
	//## super

	override def map[S,T](its:F[S])(func:S=>T):F[T]		=
		flatMap(its)(func.andThen(pure[T]))

	override def ap[S,T](func:F[S=>T])(its:F[S]):F[T]	=
		flatMap(func)(map(its)(_))
}
