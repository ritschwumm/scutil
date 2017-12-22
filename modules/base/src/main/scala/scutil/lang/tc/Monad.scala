package scutil.lang.tc

object Monad {
	def apply[F[_]](implicit ev:Monad[F]):Monad[F]	= ev
}

trait Monad[F[_]] extends Applicative[F] {
	//------------------------------------------------------------------------------
	//## own
	
	def flatMap[S,T](its:F[S])(func:S=>F[T]):F[T]
	
	//------------------------------------------------------------------------------
	//## derived
	
	def flatten[T](its:F[F[T]]):F[T]	=
			flatMap(its)(identity)
	
	//------------------------------------------------------------------------------
	//## super
	
	override def map[S,T](its:F[S])(func:S=>T):F[T]		=
			flatMap(its)(func andThen pure[T])
		
	def ap[S,T](its:F[S])(func:F[S=>T]):F[T]	=
			flatMap(func)(map(its)(_))
}
