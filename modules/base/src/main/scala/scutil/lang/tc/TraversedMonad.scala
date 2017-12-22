package scutil.lang.tc

trait TraversedMonad[F[_]]
		extends Monad[F]
		with Traversed[F]
