package scutil.lang.pimp

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
	
	def leftEffect(fx:S=>Unit):Either[S,T]	= { peer.left.toOption	foreach fx; peer }
	def rightEffect(fx:T=>Unit):Either[S,T]	= { peer.right.toOption	foreach fx; peer }
	
	def bimap[SS,TT](leftFunc:S=>SS, rightFunc:T=>TT):Either[SS,TT]	=
			peer match {
				case Left(x)	=> Left(leftFunc(x))
				case Right(x)	=> Right(rightFunc(x))
			}
	
	def toTry(implicit ev:S=>Throwable):Try[T]	=
			peer match {
				case Left(it)	=> Failure(it)
				case Right(it)	=> Success(it)
			}
			
	def toTried:Tried[S,T]	=
			Tried fromEither peer
			
	def toValidated:Validated[S,T]	=
			Validated fromEither peer
}
