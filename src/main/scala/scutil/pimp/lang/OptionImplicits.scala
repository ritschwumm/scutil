package scutil.pimp

import scala.collection.generic.CanBuildFrom

import scutil.lang._

object OptionImplicits extends OptionImplicits

trait OptionImplicits {
	implicit def toOptionExt[T](peer:Option[T]) = new OptionExt[T](peer)
}

final class OptionExt[T](peer:Option[T]) {
	def getOrError(s:String)	= peer getOrElse (sys error s)
		
	def cata[X](none: => X, some:T => X):X = 
		peer match {
			case Some(x)	=> some(x)
			case None		=> none
		}
	
	/** ap of the monad, <*> of the applicative functor */
	def ap[U,V](source:Option[U])(implicit witness:T=>U=>V):Option[V] =
			for { f	<- peer; s	<- source } yield f(s)
		
	/** ap with inverted parameters, aka <**> */
	def pa[U](func:Option[T=>U]):Option[U] =
			for { f	<- func; s	<- peer } yield f(s)
	
	/*
	//* the flatten method defined on Iterable is useless 
	def flatten[U](implicit witness:PFunction[T,U]):Option[U] =
			peer flatMap witness
	*/
			
	/** the partition method defined on Iterable is useless */
	def partition(pred:Predicate[T]):(Option[T],Option[T])	=
			(peer filter pred, peer filterNot pred)
		
	/** the zip method defined on Iterable is useless */
	def zip[U](that:Option[U]):Option[(T,U)]	=
			(peer, that) match {
				case ((Some(t),Some(u)))	=> Some((t,u))
				case _						=> None
			}
			
	def zipBy[U](func:T=>U):Option[(T,U)]	=
			peer map { it => (it,func(it)) }
		
	def zipWith[U,V](that:Option[U])(func:(T,U)=>V):Option[V]	=
			(peer,that) match {
				case ((Some(t),Some(u)))	=> Some(func(t,u))
				case _						=> None
			}
			
	/** the unzip method defined on Iterable is useless */	
	def unzip[U,V](implicit ev:T=>(U,V)):(Option[U],Option[V])	=
			peer map ev match {
				case Some((u,v))	=> (Some(u),	Some(v))
				case None			=> (None,		None)
			}
			
	def splitEither[U,V](implicit ev:T=>Either[U,V]):(Option[U],Option[V])	=
			peer map ev match {
				case Some(Left(x))	=> (Some(x),	None)
				case Some(Right(x))	=> (None,		Some(x))
				case None			=> (None,		None)
			}
	
	def splitTried[F,W](implicit ev:T=>Tried[F,W]):(Option[F],Option[W])	=
			peer map ev match {
				case Some(Fail(x))	=> (Some(x),	None)
				case Some(Win(x))	=> (None,		Some(x))
				case None			=> (None,		None)
			}
	
	/** handy replacement for opt.toSeq.flatten abusing CanBuildFrom as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:T=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			peer map ev match {
				case Some(cc)	=> cc
				case None		=> cbf().result
			}
	
	def someEffect(effect:T=>Unit):Option[T] = {
		if (peer.nonEmpty) effect(peer.get)
		peer 
	}
	
	def noneEffect(effect: =>Unit):Option[T] = {
		if (peer.isEmpty)  effect
		peer 
	}
	
	def toWin[F](fail: =>F):Tried[F,T]	= 
			peer match {
				case Some(win)	=> Win(win)
				case None		=> Fail(fail)
			} 
	
	def toFail[W](win: =>W):Tried[T,W]	= 
			peer match {
				case Some(fail)	=> Fail(fail)
				case None		=> Win(win)
			} 
	
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:T=>Option[U]):Option[Option[U]]	=
			traverseOption(identity[U])
		
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U,V](func:U=>V)(implicit ev:T=>Option[U]):Option[Option[V]]	=
			peer map ev match {
				case Some(Some(x))	=> Some(Some(func(x)))
				case Some(None)		=> None
				case None			=> Some(None)
			}
		
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def sequenceTried[F,W](implicit ev:T=>Tried[F,W]):Tried[F,Option[W]]	=
			traverseTried(identity[W])
		
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def traverseTried[F,W,V](func:W=>V)(implicit ev:T=>Tried[F,W]):Tried[F,Option[V]]	=
			peer map ev match {
				case Some(Win(x))	=> Win(Some(func(x)))
				case Some(Fail(x))	=> Fail(x)
				case None			=> Win(None)
			}
	
	def toVector:Vector[T]	= 
			peer match {
				case Some(x)	=> Vector(x)
				case None		=> Vector.empty
			}
}
