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

	@deprecated("use lift", "0.195.0")
	def mapping[A,B](func:A=>B):F[A]=>F[B]	=
		lift(func)

	def lift[A,B](func:A=>B):F[A]=>F[B]	=
		map(_)(func)

	def as[A,B](peer:F[A])(value:B):F[B]	=
		map(peer)(_ => value)

	def void[A](peer:F[A]):F[Unit]	=
		as(peer)(())

	@deprecated("use fproduct", "0.195.0")
	def zipBy[A,B](peer:F[A])(func:A=>B):F[(A,B)]		=
		fproduct(peer)(func)

	def fproduct[A,B](peer:F[A])(func:A=>B):F[(A,B)]		=
		map(peer)(it => (it, func(it)))
}
