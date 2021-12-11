package scutil.lang.tc

object Traversed extends TraversedLow {
	def apply[F[_]](using ev:Traversed[F]):Traversed[F]	= ev
}

trait TraversedLow {
	// TODO dotty is this really necessary? why doesn't inheritance provide us with an instance?
	given[F[_]](using F:TraversedMonad[F]):Traversed[F]	= F
}

trait Traversed[F[_]] extends Functor[F] {
	//------------------------------------------------------------------------------
	//## own

	def traverse[G[_],S,T](items:F[S])(func:S=>G[T])(using AP:Applicative[G]):G[F[T]]
		//sequence(map(items)(func))

	//------------------------------------------------------------------------------
	//## derived

	def sequence[G[_],T](items:F[G[T]])(using AP:Applicative[G]):G[F[T]]	=
		traverse(items)(identity)

	def flatTraverse[G[_],S,T](items:F[S])(func:S=>G[F[T]])(using AP:Applicative[G], M:Monad[F]):G[F[T]]	=
		AP.map(traverse(items)(func))(M.flatten)
}
