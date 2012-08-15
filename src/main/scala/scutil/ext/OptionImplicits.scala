package scutil.ext

import scutil.lang._
import scutil.tried._

object OptionImplicits extends OptionImplicits

trait OptionImplicits {
	implicit def toOptionExt[T](delegate:Option[T]) = new OptionExt[T](delegate)
}

final class OptionExt[T](delegate:Option[T]) {
	def getOrError(s:String)	= delegate getOrElse (sys error s)
	
	// == map some getOrElse none
	def cata[X](some:T => X, none: => X):X = delegate match {
		case Some(x)	=> some(x)
		case None		=> none
	}
	
	/** ap of the monad, <*> of the applicative functor */
	def ap[U,V](source:Option[U])(implicit witness:T=>U=>V):Option[V] =
			for { f	<- delegate; s	<- source } yield f(s)
		
	/** ap with inverted parameters, aka <**> */
	def pa[U](func:Option[T=>U]):Option[U] =
			for { f	<- func; s	<- delegate } yield f(s)
	
	/** the flatten method defined on Iterable is useless */
	def flatten[U](implicit witness:T=>Option[U]):Option[U] =
			delegate flatMap witness
			
	/** the partition method defined on Iterable is useless */
	def partition(pred:Predicate[T]):(Option[T],Option[T])	=
			(delegate filter pred, delegate filterNot pred)
		
	def someEffect(effect:T=>Unit):Option[T] = { if (delegate.nonEmpty) effect(delegate.get);	delegate }
	def noneEffect(effect: =>Unit):Option[T] = { if (delegate.isEmpty)  effect; 				delegate }
	
	def toWin[F](fail: =>F):Tried[F,T]	= cata(Win.apply, Fail(fail))
	def toFail[W](win: =>W):Tried[T,W]	= cata(Fail.apply, Win(win))
	
	def toVector:Vector[T]	= delegate match {
		case Some(x)	=> Vector(x)
		case None		=> Vector.empty
	}
}
