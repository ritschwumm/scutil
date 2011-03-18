package scutil.ext

object OptionImplicits extends OptionImplicits

trait OptionImplicits {
	implicit def toOptionExt[T](delegate:Option[T]) = new OptionExt[T](delegate)
}

final class OptionExt[T](delegate:Option[T]) {
	def getOrError(s:String) = delegate getOrElse error(s)
	
	// == map some getOrElse none
	def fold[X](some:T => X, none: => X):X = delegate match {
		case Some(x)	=> some(x)
		case None		=> none
	}
	
	/*
	// == collect f
	def filterMap[X](f:PartialFunction[T,X]):Option[X] = 
			delegate flatMap { element =>
				if (f isDefinedAt element)	Some(f apply element)
				else						None
			}
	*/
	
	/** ap of the monad , <*> of the applicative functor */
	def ap[U,V](source:Option[U])(implicit witness:T=>U=>V):Option[V] =
			for { f	<- delegate; s	<- source } yield f(s)
	
	/** the flatten method defined on Iterable returns a List, not an Option */
	def flatten[U](implicit witness:T=>Option[U]):Option[U] =
			delegate flatMap witness
			
	def noneEffect(effect: =>Unit):Option[T] = { if (delegate.isEmpty)  effect; delegate }
	def someEffect(effect: =>Unit):Option[T] = { if (delegate.nonEmpty) effect; delegate }
	def someEffect(effect:T=>Unit):Option[T] = { if (delegate.nonEmpty) effect(delegate.get); delegate }
	

}
