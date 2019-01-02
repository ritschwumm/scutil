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

	final def pureUnit:F[Unit]	= pure(())

	def combine[A,B,C](as:F[A], bs:F[B])(func:(A,B)=>C):F[C]	=
			ap(bs)(map(as)(func.curried))

	//------------------------------------------------------------------------------

	// TODO allow F[T] forall T for these?

	final def ifA(cond:Boolean)(value:F[Unit]):F[Unit]	=
			if (cond)	value
			else		pureUnit

	final def optionalA[T](value:Option[T])(action:T=>F[Unit]):F[Unit]	=
			value match {
				case Some(x)	=> action(x)
				case None		=> pureUnit
			}
}
