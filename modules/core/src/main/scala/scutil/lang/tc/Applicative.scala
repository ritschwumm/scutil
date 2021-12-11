package scutil.lang.tc

object Applicative extends ApplicativeLow {
	def apply[F[_]](using ev:Applicative[F]):Applicative[F]	= ev
}

trait ApplicativeLow {
	// TODO dotty is this really necessary? why doesn't inheritance provide us with an instance?
	given[F[_]](using F:Monad[F]):Applicative[F]	= F
}

trait Applicative[F[_]] extends Functor[F] {
	//------------------------------------------------------------------------------
	//## own

	def pure[T](it:T):F[T]
	def ap[S,T](func:F[S=>T])(its:F[S]):F[T]

	//------------------------------------------------------------------------------
	//## super

	def map[S,T](its:F[S])(func:S=>T):F[T]	=
		ap(pure(func))(its)

	//------------------------------------------------------------------------------
	//## derived

	def unit:F[Unit]	= pure(())

	def map2[A,B,C](as:F[A], bs:F[B])(func:(A,B)=>C):F[C]	=
		ap(map(as)(func.curried))(bs)

	def product[A,B](as:F[A], bs:F[B]):F[(A,B)]	=
		map2(as, bs)((a,b) => (a,b))

	def productL[A,B](as:F[A], bs:F[B]):F[A]	=
		map2(as, bs)((a,b) => a)

	def productR[A,B](as:F[A], bs:F[B]):F[B]	=
		map2(as, bs)((a,b) => b)

	//------------------------------------------------------------------------------

	// TODO allow F[T] forall T for these?

	def whenA(cond:Boolean)(value: =>F[Unit]):F[Unit]	=
		if (cond)	value
		else		unit

	def unlessA(cond:Boolean)(value: =>F[Unit]):F[Unit]	=
		whenA(!cond)(value)

	def optionalA[T](value:Option[T])(action:T=>F[Unit]):F[Unit]	=
		value match {
			case Some(x)	=> action(x)
			case None		=> unit
		}
}
