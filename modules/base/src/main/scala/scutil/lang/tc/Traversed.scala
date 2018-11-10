package scutil.lang.tc

object Traversed {
	def apply[F[_]](implicit ev:Traversed[F]):Traversed[F]	= ev
}

trait Traversed[F[_]] extends Functor[F] {
	def traverse[G[_],S,T](it:F[S])(func:S=>G[T])(implicit AP:Applicative[G]):G[F[T]]
			//sequence(map(it)(func))

	def sequence[G[_],T](it:F[G[T]])(implicit AP:Applicative[G]):G[F[T]]	=
			traverse(it)(identity)
}
