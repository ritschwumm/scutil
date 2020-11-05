package scutil.lang.tc

object Functor {
	def apply[F[_]](implicit ev:Functor[F]):Functor[F]	= ev
}

trait Functor[F[_]] {
	//------------------------------------------------------------------------------
	//## own

	def map[A,B](peer:F[A])(func:A=>B):F[B]

	//------------------------------------------------------------------------------
	//## derived

	def mapping[A,B](func:A=>B):F[A]=>F[B]	=
		map(_)(func)

	def as[A,B](peer:F[A])(value:B):F[B]	=
		map(peer)(_ => value)

	def void[A](peer:F[A]):F[Unit]	=
		as(peer)(())

	def zipBy[A,B](peer:F[A])(func:A=>B):F[(A,B)]		=
		map(peer)(it => (it, func(it)))
}
