package scutil.lang.tc

object Functor {
	def apply[F[_]](implicit ev:Functor[F]):Functor[F]	= ev
}

trait Functor[F[_]] {
	def map[A,B](it:F[A])(func:A=>B):F[B]
	
	final def mapper[A,B](func:A=>B):F[A]=>F[B]	=
			map(_)(func)
}
