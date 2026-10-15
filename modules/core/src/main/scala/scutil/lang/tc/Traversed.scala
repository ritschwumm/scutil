package scutil.lang.tc

object Traversed {
	def apply[F[_]:Traversed as F]:Traversed[F]	= F
}

trait Traversed[F[_]] extends Functor[F] {
	//------------------------------------------------------------------------------
	//## own

	def traverse[G[_]:Applicative as AP,S,T](items:F[S])(func:S=>G[T]):G[F[T]]
		//sequence(map(items)(func))

	//------------------------------------------------------------------------------
	//## derived

	def sequence[G[_]:Applicative as AP,T](items:F[G[T]]):G[F[T]]	=
		traverse(items)(identity)

	def flatTraverse[G[_]:Applicative as AP,S,T](items:F[S])(func:S=>G[F[T]])(using M:Monad[F]):G[F[T]]	=
		AP.map(traverse(items)(func))(M.flatten)

	def traverseVoid[G[_]:Applicative as AP,S,T](items:F[S])(func:S=>G[T])	=
		AP.void(traverse(items)(func))
}
