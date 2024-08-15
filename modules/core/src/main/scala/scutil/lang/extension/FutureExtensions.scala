package scutil.lang.extension

import scala.concurrent.*
import scala.util.{ Try, Success, Failure }

object FutureExtensions {
	implicit final class FutureExt[T](peer:Future[T]) {
		def map2[U,V](that:Future[U])(func:(T,U)=>V)(using executor:ExecutionContext):Future[V]	=
			peer.zip(that).map(func.tupled)

		// TODO is this correct, or should we use flatMap?
		def product[U](that:Future[U]):Future[(T,U)]	=
			peer `zip` that

		// TODO is this correct, or should we use flatMap?
		def ap[U,V](that:Future[U])(using executor:ExecutionContext, ev:T <:< (U=>V)):Future[V]	=
			peer.zip(that).map { (u2v, u) => u2v(u) }

		//------------------------------------------------------------------------------

		def andThenTotal[U](func:Try[T]=>U)(using executor:ExecutionContext):Future[T]	=
			peer.andThen{ case x => func(x) }

		/** like transform, but each branch can fail independently */
		def transformTry[U](failure:Throwable=>Try[U], success:T=>Try[U])(using executor:ExecutionContext):Future[U]	= {
			peer transform {
				case Failure(t)	=> failure(t)
				case Success(t)	=> success(t)
			}
		}

		//------------------------------------------------------------------------------

		/** always succeeds wrapping the original success/failure in a Tried */
		def wrapEither(using executor:ExecutionContext):Future[Either[Throwable,T]]	=
			peer transform { it => Try(it.toEither) }

		/** succeeds for a Win, fails for a Fail */
		def unwrapEither[X](using executor:ExecutionContext, ev:T <:< Either[Throwable,X]):Future[X]	=
			peer transform(_.flatMap { ev(_).toTry })

		def mapEither[X](func:Either[Throwable,T]=>Either[Throwable,X])(using executor:ExecutionContext):Future[X]	=
			peer transform { it => func(it.toEither).toTry }
	}
}
