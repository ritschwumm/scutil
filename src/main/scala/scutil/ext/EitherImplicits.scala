package scutil.ext

import scala.util.{ Try, Success, Failure }

import scutil.tried._

object EitherImplicits extends EitherImplicits

trait EitherImplicits {
	implicit def toEitherExt[S,T](delegate:Either[S,T]) = new EitherExt[S,T](delegate)
}

final class EitherExt[S,T](delegate:Either[S,T]) {
	def cata[U](left:S=>U, right:T=>U)			= delegate fold (left, right)
	
	@deprecated("use cata", "0.23.0")
	def cataSwapped[U](right:T=>U, left:S=>U)	= cata(left, right)
			
	def leftOrError(s:String)	= delegate.left		getOrElse (sys error s)
	def rightOrError(s:String)	= delegate.right	getOrElse (sys error s)
	
	def leftEffect(fx:S=>Unit):Either[S,T]	= { if (delegate.isLeft)  fx(delegate.left.get);	delegate }
	def rightEffect(fx:T=>Unit):Either[S,T]	= { if (delegate.isRight) fx(delegate.right.get);	delegate }
	
	def toTry(implicit ev:S=>Throwable):Try[T]	= 
			delegate match {
				case Left(it)	=> Failure(it)
				case Right(it)	=> Success(it)
			}
			
	def toTried:Tried[S,T]	= 
			delegate match {
				case Left(it)	=> Fail(it)
				case Right(it)	=> Win(it)
			}
}
