package scutil.pimp

import scala.concurrent._

import scutil.lang._

object FutureImplicits extends FutureImplicits

trait FutureImplicits {
	implicit def toFutureExt[T](peer:Future[T])	= new FutureExt(peer)
}

final class FutureExt[T](peer:Future[T]) {
	def zipWith[U,V](that:Future[U])(func:(T,U)=>V)(implicit executor:ExecutionContext):Future[V]	=
			peer zip that map func.tupled
		
	def pa[U](that:Future[T=>U])(implicit executor:ExecutionContext):Future[U]	=
			peer zip that map { case (t, t2u) => t2u(t) }
		
	def ap[U,V](that:Future[U])(implicit ev:T=>U=>V, executor:ExecutionContext):Future[V]	=
			peer zip that map { case (u2v, u) => u2v(u) }
}
