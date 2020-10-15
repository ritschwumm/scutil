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
	//## super

	def map[S,T](its:F[S])(func:S=>T):F[T]	=
		ap(its)(pure(func))

	//------------------------------------------------------------------------------
	//## derived

	@deprecated("use unit", "0.181.0")
	final def pureUnit:F[Unit]	= pure(())

	def unit:F[Unit]	= pure(())

	@deprecated("use map2", "0.181.0")
	def combine[A,B,C](as:F[A], bs:F[B])(func:(A,B)=>C):F[C]	=
		ap(bs)(map(as)(func.curried))

	def map2[A,B,C](as:F[A], bs:F[B])(func:(A,B)=>C):F[C]	=
		ap(bs)(map(as)(func.curried))

	def tuple[A,B](as:F[A], bs:F[B]):F[(A,B)]	=
		map2(as, bs)((a,b) => (a,b))

	def first[A,B](as:F[A], bs:F[B]):F[A]	=
		map2(as, bs)((a,b) => a)

	def second[A,B](as:F[A], bs:F[B]):F[B]	=
		map2(as, bs)((a,b) => b)

	//------------------------------------------------------------------------------

	// TODO allow F[T] forall T for these?

	def ifA(cond:Boolean)(value:F[Unit]):F[Unit]	=
		if (cond)	value
		else		unit

	def optionalA[T](value:Option[T])(action:T=>F[Unit]):F[Unit]	=
		value match {
			case Some(x)	=> action(x)
			case None		=> unit
		}
}
