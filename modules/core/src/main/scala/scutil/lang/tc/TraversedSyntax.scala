package scutil.lang.tc

object TraversedSyntax {
	implicit final class TraversedSyntaxExt[F[_],T](peer:F[T]) {
		// NOTE dotty if ev and AP are swapped we get an "Ambiguous given instances" error at usage site
		def sequence[G[_],U](using TR:Traversed[F], ev:T <:< G[U], AP:Applicative[G]):G[F[U]]	=
			traverse(ev)

		def traverse[G[_],U](func:T=>G[U])(using TR:Traversed[F], AP:Applicative[G]):G[F[U]]	=
			TR.traverse(peer)(func)

		def flatTraverse[G[_],U](func:T=>G[F[U]])(using TR:Traversed[F], M:Monad[F], AP:Applicative[G]):G[F[U]]	=
			TR.flatTraverse(peer)(func)

		def traverseVoid[G[_],U](func:T=>G[U])(using TR:Traversed[F], AP:Applicative[G]):G[Unit]	=
			TR.traverseVoid(peer)(func)
	}
}
