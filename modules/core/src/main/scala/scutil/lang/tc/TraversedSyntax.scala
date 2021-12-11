package scutil.lang.tc

object TraversedSyntax {
	implicit final class TraversedSyntaxExt[F[_],T](peer:F[T]) {
		def sequence[G[_],U](using TR:Traversed[F], AP:Applicative[G])(implicit ev: T <:< G[U]):G[F[U]]	=
			traverse(ev)

		def traverse[G[_],U](func:T=>G[U])(using TR:Traversed[F], AP:Applicative[G]):G[F[U]]	=
			TR.traverse(peer)(func)

		def flatTraverse[G[_],U](func:T=>G[F[U]])(using TR:Traversed[F], M:Monad[F], AP:Applicative[G]):G[F[U]]	=
			TR.flatTraverse(peer)(func)
	}
}
