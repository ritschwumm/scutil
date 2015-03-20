package scutil.lang.pimp

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
	// the flatten method defined on Iterable is useless
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
			
	def partitionEither[U,V](implicit ev:T=>Either[U,V]):(Option[U],Option[V])	=
			peer map ev match {
				case Some(Left(x))	=> (Some(x),	None)
				case Some(Right(x))	=> (None,		Some(x))
				case None			=> (None,		None)
			}
	
	def partitionTried[F,W](implicit ev:T=>Tried[F,W]):(Option[F],Option[W])	=
			peer map ev match {
				case Some(Fail(x))	=> (Some(x),	None)
				case Some(Win(x))	=> (None,		Some(x))
				case None			=> (None,		None)
			}
	
	/** handy replacement for opt.toISeq flatMap func abusing CanBuildFrom as a Zero typeclass */
	def flatMapMany[U,CC[_]](func:T=>CC[U])(implicit cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			peer map func match {
				case Some(cc)	=> cc
				case None		=> cbf().result
			}
			
	/** handy replacement for opt.toISeq.flatten abusing CanBuildFrom as a Zero typeclass */
	def flattenMany[U,CC[_]](implicit ev:T=>CC[U], cbf:CanBuildFrom[CC[U],U,CC[U]]):CC[U]	=
			flatMapMany(ev)
			
	//------------------------------------------------------------------------------
	
	def someEffect(effect:T=>Unit):Option[T] = {
		if (peer.nonEmpty) effect(peer.get)
		peer
	}
	
	def noneEffect(effect: =>Unit):Option[T] = {
		if (peer.isEmpty)  effect
		peer
	}
	
	//------------------------------------------------------------------------------
	
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def sequenceOption[U](implicit ev:PFunction[T,U]):Option[Option[U]]	=
			traverseOption(ev)
		
	/** peer is traversable (in the haskell sense), Option is an idiom. */
	def traverseOption[U](func:PFunction[T,U]):Option[Option[U]]	=
			peer map func match {
				case None		=> Some(None)
				case Some(None)	=> None
				case x			=> x
			}
			
	/** peer is traversable (in the haskell sense), ISeq is an idiom. */
	def sequenceISeq[U](implicit ev:T=>ISeq[U]):ISeq[Option[U]]	=
			traverseISeq(ev)
		
	/** peer is traversable (in the haskell sense), ISeq is an idiom. */
	def traverseISeq[U](func:T=>ISeq[U]):ISeq[Option[U]]	=
			peer map func match {
				case None		=> ISeq(None)
				case Some(xs)	=> xs map Some.apply
			}
			
	/** peer is traversable (in the haskell sense), ISeq is an idiom. */
	def sequenceTraversable[CC[_]<:Traversable[U],U](implicit ev:T=>CC[U], cbf:CanBuildFrom[CC[T],Option[U],CC[Option[U]]]):CC[Option[U]]	=
			traverseTraversable(ev)
		
	/** peer is traversable (in the haskell sense), ISeq is an idiom. */
	def traverseTraversable[CC[_]<:Traversable[U],U](func:T=>CC[U])(implicit cbf:CanBuildFrom[CC[T],Option[U],CC[Option[U]]]):CC[Option[U]]	= {
		val builder	= cbf()
		peer map func match {
			case None		=> builder.result
			case Some(xs)	=> xs foreach { x => builder += Some(x) }
		}
		builder.result
	}
		
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def sequenceTried[F,W](implicit ev:T=>Tried[F,W]):Tried[F,Option[W]]	=
			traverseTried(ev)
		
	/** peer is traversable (in the haskell sense), Tried is an idiom. */
	def traverseTried[F,W](func:T=>Tried[F,W]):Tried[F,Option[W]]	=
			peer map func match {
				case None			=> Win(None)
				case Some(Fail(x))	=> Fail(x)
				case Some(Win(x))	=> Win(Some(x))
			}
			
	/** peer is traversable (in the haskell sense), Validated is an idiom. */
	def sequenceValidated[F,W](implicit ev:T=>Validated[F,W]):Validated[F,Option[W]]	=
			traverseValidated(ev)
		
	/** peer is traversable (in the haskell sense), Validated is an idiom. */
	def traverseValidated[F,W](func:T=>Validated[F,W]):Validated[F,Option[W]]	=
			peer map func match {
				case None			=> Good(None)
				case Some(Bad(x))	=> Bad(x)
				case Some(Good(x))	=> Good(Some(x))
			}
			
	//------------------------------------------------------------------------------
	
	def toWin[F](fail: =>F):Tried[F,T]	=
			Tried winOr (peer, fail)
	
	def toFail[W](win: =>W):Tried[T,W]	=
			Tried failOr (peer, win)
			
	def toGood[F](problems: =>Nes[F]):Validated[F,T]	=
			Validated goodOr (peer, problems)
		
	def toBad[ES,W](good: =>W)(implicit ev:T=>Nes[ES]):Validated[ES,W]	=
			Validated badOr (peer map ev, good)
		
	def toGood1[F](problem: =>F):Validated[F,T]	=
			Validated goodOr (peer, Nes single problem)
		
	def toBad1[ES,W](good: =>W)(implicit ev:T=>ES):Validated[ES,W]	=
			Validated badOr (peer map (ev andThen Nes.single), good)
		
	def toISeq:ISeq[T]	=
			toVector
			
	def toVector:Vector[T]	=
			peer match {
				case Some(x)	=> Vector(x)
				case None		=> Vector.empty
			}
}
