package scutil.lang.tc

import scutil.lang.Identity

object Functor extends FunctorLow1 {
	def apply[F[_]](using ev:Functor[F]):Functor[F]	= ev

	//------------------------------------------------------------------------------

	given [S]:Functor[Function[S,_]]	=
		new Functor[Function[S,_]] {
			def map[A,B](it:Function[S,A])(func:A=>B):Function[S,B]	= it.andThen(func)
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
			def map[A,B](it:S=>Option[A])(func:A=>B):S=>Option[B]		= it(_).map(func)
		}
	*/

	// TODO questionable
	given [F[_]:Functor,S]:Functor[[X] =>> S=>F[X]]	=
		new Functor[[X] =>> S=>F[X]] {
			def map[A,B](it:S=>F[A])(func:A=>B):S=>F[B]	=
				a => Functor[F].map(it(a))(func)
		}

	//-----------------------------------------------------------------------------

	given OptionTraversedMonad:TraversedMonad[Option]	=
		new TraversedMonad[Option] {
			override def map[A,B](it:Option[A])(func:A=>B):Option[B]				= it.map(func)

			override def pure[A](it:A):Option[A]									= Some(it)
			override def flatMap[A,B](it:Option[A])(func:A=>Option[B]):Option[B]	= it.flatMap(func)

			override def traverse[G[_],S,T](it:Option[S])(func:S=>G[T])(using AP:Applicative[G]):G[Option[T]]	=
				it match {
					case None		=> AP.pure(None)
					case Some(s)	=> AP.map(func(s))(Some.apply)
				}
		}

	given EitherTraversedMonad[S]:TraversedMonad[Either[S,_]]	=
		new TraversedMonad[Either[S,_]] {
			override def pure[A](it:A):Either[S,A]										= Right(it)
			override def map[A,B](it:Either[S,A])(func:A=>B):Either[S,B]				= it.map(func)
			override def flatMap[A,B](it:Either[S,A])(func:A=>Either[S,B]):Either[S,B]	= it.flatMap(func)

			override def traverse[G[_],A,B](it:Either[S,A])(func:A=>G[B])(using AP:Applicative[G]):G[Either[S,B]]	=
				it match {
					case Left(x)	=> AP.pure(Left(x))
					case Right(x)	=> AP.map(func(x))(Right.apply)
				}
		}

	given VectorTraversedMonad:TraversedMonad[Vector]	=
		new TraversedMonad[Vector] {
			override def map[A,B](it:Vector[A])(func:A=>B):Vector[B]				= it.map(func)
			override def pure[A](it:A):Vector[A]									= Vector(it)
			override def flatMap[A,B](it:Vector[A])(func:A=>Vector[B]):Vector[B]	= it.flatMap(func)
			override def traverse[G[_],S,T](it:Vector[S])(func:S=>G[T])(using AP:Applicative[G]):G[Vector[T]]	=
				it.map(func).foldLeft(AP.pure(Vector.empty[T])) {
					(xs, x) => AP.map2(xs, x)(_ :+ _)
				}
		}

	given ListTraversedMonad:TraversedMonad[List]	=
		new TraversedMonad[List] {
			override def map[A,B](it:List[A])(func:A=>B):List[B]			= it.map(func)
			override def pure[A](it:A):List[A]								= List(it)
			override def flatMap[A,B](it:List[A])(func:A=>List[B]):List[B]	= it.flatMap(func)
			override def traverse[G[_],S,T](it:List[S])(func:S=>G[T])(using AP:Applicative[G]):G[List[T]]	= {
				val mapped		= it.map(func)
				val empty		= AP.pure(List.empty[T])
				val reversed	=
					mapped.foldLeft(empty) { (xs, x) =>
						AP.map2(xs, x) { (a,b) =>
							// builds in reverse order because appending to a list is too expensive
							b :: a
						}
					}
				// reverse the effect of building in reverse
				AP.map(reversed)(_.reverse)
			}
		}
}

trait FunctorLow1 extends FunctorLow2 {
	given SeqTraversedMonad:TraversedMonad[Seq]	=
		new TraversedMonad[Seq] {
			override def map[A,B](it:Seq[A])(func:A=>B):Seq[B]			= it.map(func)
			override def pure[A](it:A):Seq[A]							= Seq(it)
			override def flatMap[A,B](it:Seq[A])(func:A=>Seq[B]):Seq[B]	= it.flatMap(func)
			override def traverse[G[_],S,T](it:Seq[S])(func:S=>G[T])(using AP:Applicative[G]):G[Seq[T]]	=
				it.map(func).foldLeft(AP.pure((Vector.empty[T]):Seq[T])) {
					(xs, x) => AP.map2(xs, x)(_ :+ _)
				}
		}
}

trait FunctorLow2 {
	given IdentityTraversedMonad:TraversedMonad[Identity]	=
		new TraversedMonad[Identity] {
			override def map[A,B](it:Identity[A])(func:A=>B):Identity[B]				= func(it)
			override def pure[A](it:A):Identity[A]										= it
			override def flatMap[A,B](it:Identity[A])(func:A=>Identity[B]):Identity[B]	= func(it)
			override def traverse[G[_],S,T](it:Identity[S])(func:S=>G[T])(using AP:Applicative[G]):G[Identity[T]]	= func(it)
		}
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

trait TraversedMonad[F[_]] extends Monad[F], Traversed[F]
