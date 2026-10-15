package scutil.lang.tc

object TraversedSyntax {
	extension [F[_],T](peer:F[T]) {
		// NOTE dotty if ev and AP are swapped we get an "Ambiguous given instances" error at usage site
		def sequence[G[_]:Applicative as AP,U](using TR:Traversed[F], ev:T <:< G[U]):G[F[U]]	=
			traverse(ev)

		def sequenceVoid[G[_]:Applicative as AP,U](using TR:Traversed[F], ev:T <:< G[U]):G[Unit]	=
			traverseVoid(ev)

		def traverse[G[_]:Applicative as AP,U](func:T=>G[U])(using TR:Traversed[F]):G[F[U]]	=
			TR.traverse(peer)(func)

		def flatTraverse[G[_]:Applicative as AP,U](func:T=>G[F[U]])(using TR:Traversed[F], M:Monad[F]):G[F[U]]	=
			TR.flatTraverse(peer)(func)

		def traverseVoid[G[_]:Applicative as AP,U](func:T=>G[U])(using TR:Traversed[F]):G[Unit]	=
			TR.traverseVoid(peer)(func)
	}
}
