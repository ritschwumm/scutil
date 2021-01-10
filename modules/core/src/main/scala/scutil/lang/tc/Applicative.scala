package scutil.lang.tc

object Applicative {
	def apply[F[_]](implicit ev:Applicative[F]):Applicative[F]	= ev
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

	@deprecated("use product", "0.195.0")
	def tuple[A,B](as:F[A], bs:F[B]):F[(A,B)]	=
		product(as, bs)

	def product[A,B](as:F[A], bs:F[B]):F[(A,B)]	=
		map2(as, bs)((a,b) => (a,b))

	@deprecated("use productL", "0.195.0")
	def first[A,B](as:F[A], bs:F[B]):F[A]	=
		productL(as, bs)

	def productL[A,B](as:F[A], bs:F[B]):F[A]	=
		map2(as, bs)((a,b) => a)

	@deprecated("use productR", "0.195.0")
	def second[A,B](as:F[A], bs:F[B]):F[B]	=
		productR(as, bs)

	def productR[A,B](as:F[A], bs:F[B]):F[B]	=
		map2(as, bs)((a,b) => b)

	//------------------------------------------------------------------------------

	// TODO allow F[T] forall T for these?

	@deprecated("use whenA", "0.195.0")
	def ifA(cond:Boolean)(value:F[Unit]):F[Unit]	=
		if (cond)	value
		else		unit

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
