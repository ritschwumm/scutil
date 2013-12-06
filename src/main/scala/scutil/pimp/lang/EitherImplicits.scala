package scutil.pimp

import scala.util.{ Try, Success, Failure }

import scutil.lang._

object EitherImplicits extends EitherImplicits

trait EitherImplicits {
	implicit def toEitherExt[S,T](peer:Either[S,T]) = new EitherExt[S,T](peer)
}

final class EitherExt[S,T](peer:Either[S,T]) {
	def cata[U](left:S=>U, right:T=>U):U	= peer fold (left, right)
	
	def leftOrError(s:String)	= peer.left		getOrElse (sys error s)
	def rightOrError(s:String)	= peer.right	getOrElse (sys error s)
	
	def leftEffect(fx:S=>Unit):Either[S,T]	= { if (peer.isLeft)  fx(peer.left.get);	peer }
	def rightEffect(fx:T=>Unit):Either[S,T]	= { if (peer.isRight) fx(peer.right.get);	peer }
	
	def toTry(implicit ev:S=>Throwable):Try[T]	= 
			peer match {
				case Left(it)	=> Failure(it)
				case Right(it)	=> Success(it)
			}
			
	def toTried:Tried[S,T]	= 
			peer match {
				case Left(it)	=> Fail(it)
				case Right(it)	=> Win(it)
			}
}
