package scutil.lang.tc

object MonadSyntax extends MonadSyntax

trait MonadSyntax {
	implicit class MonadValueSyntaxExt[F[_],T](peer:F[T])(implicit MF:Monad[F]) {
		def flatMap[U](func:T=>F[U]):F[U]			= (MF flatMap peer)(func)
		def flatten[U](implicit func:T=>F[U]):F[U]	= flatMap(func)
		
		// aka >>
		def first[U](that:F[U]):F[T]	=
			(MF flatMap peer) { t =>
				(MF map that) { u =>
					t
				}
			}
		def second[U](that:F[U]):F[U]	=
			(MF flatMap peer) { t =>
				(MF map that) { u =>
					u
				}
			}
	}
	
	implicit class MonadArrowSyntax[F[_],S,T](peer:S=>F[T])(implicit MF:Monad[F]) {
		// aka liftM
		val flatMapping:F[S]=>F[T]	= fs => MF.flatMap(fs)(peer)
	}
}
