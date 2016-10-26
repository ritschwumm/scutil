package scutil.lang.pimp

import scala.concurrent._
import scala.util.{ Try, Success, Failure }

import scutil.lang._
import scutil.lang.pimp.TryImplicits._

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
		
	//------------------------------------------------------------------------------
	
	def andThenTotal[U](func:Try[T]=>U)(implicit executor:ExecutionContext):Future[T]	=
			peer andThen { case x => func(x) }
		
	def mapTry[U](func:Try[T]=>Try[U])(implicit executor:ExecutionContext):Future[U]	= {
		val p = Promise[U]
		peer onComplete { candidate =>
			func(candidate) match {
				case Failure(t) => p failure t
				case Success(v) => p success v
			}
		}
		p.future
	}
	
	def flatMapTry[U](func:Try[T]=>Future[U])(implicit executor:ExecutionContext):Future[U]	= {
		val p = Promise[U]
		peer onComplete { candidate =>
			func(candidate) onComplete {
				case Failure(t) => p failure t
				case Success(v) => p success v
			}
		}
		p.future
	}
	
	/** like transform, but each branch can fail independently */
	def transformTry[U](failure:Throwable=>Try[U], success:T=>Try[U])(implicit executor:ExecutionContext):Future[U]	= {
		mapTry {
			case Failure(t)	=> failure(t)
			case Success(t)	=> success(t)
		}
	}
	
	//------------------------------------------------------------------------------
	
	/** always succeeds wrapping the original success/failure in a Tried */
	def wrapTried(implicit executor:ExecutionContext):Future[Tried[Throwable,T]]	=
			mapTry { it => Try(it.toTried) }
	
	/** succeeds for a Win, fails for a Fail */
	def unwrapTried[X](implicit ev:T=>Tried[Throwable,X], executor:ExecutionContext):Future[X]	=
			mapTry { _ flatMap { ev(_).toTry } }
		
	def mapTried[X](func:Tried[Throwable,T]=>Tried[Throwable,X])(implicit executor:ExecutionContext):Future[X]	=
			mapTry { it => func(it.toTried).toTry }
}
