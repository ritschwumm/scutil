package scutil.concurrent.pimp

import scala.concurrent._
import scala.util.{ Try, Success, Failure }

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
}
