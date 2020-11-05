package scutil.lang.tc

object Traversed {
	def apply[F[_]](implicit ev:Traversed[F]):Traversed[F]	= ev
}

trait Traversed[F[_]] extends Functor[F] {
	//------------------------------------------------------------------------------
	//## own

	def traverse[G[_],S,T](items:F[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[F[T]]
		//sequence(map(items)(func))

	//------------------------------------------------------------------------------
	//## derived

	def sequence[G[_],T](items:F[G[T]])(implicit AP:Applicative[G]):G[F[T]]	=
		traverse(items)(identity)

	def flatTraverse[G[_],S,T](items:F[S])(func:S=>G[F[T]])(implicit AP:Applicative[G], M:Monad[F]):G[F[T]]	=
		AP.map(traverse(items)(func))(M.flatten)
}
