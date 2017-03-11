package scutil.lang.tc

object Applicative {
	def apply[F[_]](implicit ev:Applicative[F]):Applicative[F]	= ev
}

trait Applicative[F[_]] extends Functor[F] {
	//------------------------------------------------------------------------------
	//## own
	
	def pure[T](it:T):F[T]
	def ap[S,T](its:F[S])(func:F[S=>T]):F[T]
	
	//------------------------------------------------------------------------------
	//## derived
	
	def combine[A,B,C](as:F[A], bs:F[B])(func:(A,B)=>C):F[C]	=
			ap(bs)(map(as)(func.curried))
	
	//------------------------------------------------------------------------------
	//## super
	
	def map[S,T](its:F[S])(func:S=>T):F[T]	=
			ap(its)(pure(func))
}
