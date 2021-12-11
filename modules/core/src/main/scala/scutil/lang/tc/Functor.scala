package scutil.lang.tc

object Functor extends FunctorLow1 {
	def apply[F[_]](using ev:Functor[F]):Functor[F]	= ev

	//------------------------------------------------------------------------------

	given [S]:Functor[Function[S,_]]	=
		new Functor[Function[S,_]] {
			def map[A,B](it:Function[S,A])(func:A=>B):Function[S,B]	= it andThen func
		}

	// TODO can we have a TraversedMonad here?
	given [S]:Functor[(S,_)]	=
		new Functor[(S,_)] {
			def map[A,B](it:(S,A))(func:A=>B):(S,B)	= (it._1, func(it._2))
		}

	/*
	// TODO questionable
	// TODO do we get this for free with FFunctionFunctor*
	// NOTE PFunction and FFunction are both just ReaderT/Kleisli
	given [S]:Functor[[X] =>> S=>Option[X]]	=
		new Functor[[X] =>> S=>Option[X]] {
			def map[A,B](it:S=>Option[A])(func:A=>B):S=>Option[B]		= it(_) map func
		}
	*/

	// TODO questionable
	given [F[_]:Functor,S]:Functor[[X] =>> S=>F[X]]	=
		new Functor[[X] =>> S=>F[X]] {
			def map[A,B](it:S=>F[A])(func:A=>B):S=>F[B]	=
					a => (Functor[F] map it(a))(func)
		}
}

trait FunctorLow1 extends FunctorLow2 {
	// TODO dotty is this really necessary? why doesn't inheritance provide us with an instance?
	given[F[_]](using F:Applicative[F]):Functor[F]	= F
}

trait FunctorLow2 {
	// TODO dotty is this really necessary? why doesn't inheritance provide us with an instance?
	given[F[_]](using F:Traversed[F]):Functor[F]	= F
}

trait Functor[F[_]] {
	//------------------------------------------------------------------------------
	//## own

	def map[A,B](peer:F[A])(func:A=>B):F[B]

	//------------------------------------------------------------------------------
	//## derived

	def lift[A,B](func:A=>B):F[A]=>F[B]	=
		map(_)(func)

	def as[A,B](peer:F[A])(value:B):F[B]	=
		map(peer)(_ => value)

	def void[A](peer:F[A]):F[Unit]	=
		as(peer)(())

	def fproduct[A,B](peer:F[A])(func:A=>B):F[(A,B)]		=
		map(peer)(it => (it, func(it)))
}
