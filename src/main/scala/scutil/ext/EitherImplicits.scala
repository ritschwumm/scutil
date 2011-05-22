package scutil.ext

object EitherImplicits extends EitherImplicits

trait EitherImplicits {
	implicit def toEitherExt[S,T](delegate:Either[S,T]) = new EitherExt[S,T](delegate)
}

final class EitherExt[S,T](delegate:Either[S,T]) {
	def leftOrError(s:String)	= delegate.left		getOrElse (sys error s)
	def rightOrError(s:String)	= delegate.right	getOrElse (sys error s)
	
	def leftEffect(fx: =>Unit):Either[S,T]	= { if (delegate.isLeft)  fx; delegate }
	def rightEffect(fx: =>Unit):Either[S,T]	= { if (delegate.isRight) fx; delegate }
	
	def leftEffect(fx:S=>Unit):Either[S,T]	= { if (delegate.isLeft)  fx(delegate.left.get);	delegate }
	def rightEffect(fx:T=>Unit):Either[S,T]	= { if (delegate.isRight) fx(delegate.right.get);	delegate }
}
