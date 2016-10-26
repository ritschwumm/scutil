package scutil.lang.tc

object CanMap extends CanMapInstances {
	def apply[F[_]:CanMap]:CanMap[F]	= implicitly[CanMap[F]]
}

/** aka Functor */
trait CanMap[F[_]] {
	def map[A,B](it:F[A])(func:A=>B):F[B]
	
	final def mapper[A,B](func:A=>B):F[A]=>F[B]	=
			map(_)(func)
}
