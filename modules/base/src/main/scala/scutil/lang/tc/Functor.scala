package scutil.lang.tc

object Functor {
	def apply[F[_]](implicit ev:Functor[F]):Functor[F]	= ev
}

trait Functor[F[_]] {
	//------------------------------------------------------------------------------
	//## own

	def map[A,B](it:F[A])(func:A=>B):F[B]

	//------------------------------------------------------------------------------
	//## derived

	def mapper[A,B](func:A=>B):F[A]=>F[B]	=
		map(_)(func)
}
