package scutil.lang.tc

trait TraversedSyntax {
	implicit class TraversedSyntaxExt[F[_],T](peer:F[T]) {
		def sequence[G[_],U](implicit ev:T=>G[U], TR:Traversed[F], AP:Applicative[G]):G[F[U]]	=
			traverse(ev)

		def traverse[G[_],U](func:T=>G[U])(implicit TR:Traversed[F], AP:Applicative[G]):G[F[U]]	=
			(TR traverse peer)(func)
	}
}
