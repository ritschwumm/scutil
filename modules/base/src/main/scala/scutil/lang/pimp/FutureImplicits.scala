package scutil.lang.pimp

import scala.concurrent._
import scala.util.{ Try, Success, Failure }

object FutureImplicits extends FutureImplicits

trait FutureImplicits {
	@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
	implicit final class FutureExt[T](peer:Future[T]) {
		def zipWith[U,V](that:Future[U])(func:(T,U)=>V)(implicit executor:ExecutionContext):Future[V]	=
			peer zip that map func.tupled

		def pa[U](that:Future[T=>U])(implicit executor:ExecutionContext):Future[U]	=
			peer zip that map { case (t, t2u) => t2u(t) }

		def ap[U,V](that:Future[U])(implicit ev:T=>U=>V, executor:ExecutionContext):Future[V]	=
			peer zip that map { case (u2v, u) => u2v(u) }

		//------------------------------------------------------------------------------

		def andThenTotal[U](func:Try[T]=>U)(implicit executor:ExecutionContext):Future[T]	=
			peer andThen { case x => func(x) }

		/** like transform, but each branch can fail independently */
		def transformTry[U](failure:Throwable=>Try[U], success:T=>Try[U])(implicit executor:ExecutionContext):Future[U]	= {
			peer transform {
				case Failure(t)	=> failure(t)
				case Success(t)	=> success(t)
			}
		}

		//------------------------------------------------------------------------------

		/** always succeeds wrapping the original success/failure in a Tried */
		def wrapEither(implicit executor:ExecutionContext):Future[Either[Throwable,T]]	=
			peer transform { it => Try(it.toEither) }

		/** succeeds for a Win, fails for a Fail */
		def unwrapEither[X](implicit ev:T=>Either[Throwable,X], executor:ExecutionContext):Future[X]	=
			peer transform { _ flatMap { ev(_).toTry } }

		def mapEither[X](func:Either[Throwable,T]=>Either[Throwable,X])(implicit executor:ExecutionContext):Future[X]	=
			peer transform { it => func(it.toEither).toTry }
	}
}
