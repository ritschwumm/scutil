package scutil.pimp

import scutil.lang._

object PairImplicits extends PairImplicits

trait PairImplicits {
	implicit def toPairExt[T1,T2](peer:Pair[T1,T2]) = new PairExt[T1,T2](peer)
}

final class PairExt[T1,T2](peer:Pair[T1,T2]) {
	def first:FirstProjection[T1,T2]	= new FirstProjection[T1,T2](peer)
	def second:SecondProjection[T1,T2]	= new SecondProjection[T1,T2](peer)
	
	/** apply function in first element to value in second */
	def applied[U](implicit ev:T1=>(T2=>U)):U		= ev(peer._1)(peer._2)
	/** apply function in second element to value in first */
	def appliedSwap[U](implicit ev:T2=>(T1=>U)):U	= ev(peer._2)(peer._1)
	
	/** apply binary function to elements */
	def combineWith[U](func:(T1,T2)=>U):U	= func(peer._1, peer._2)
}

/** comonad, see Either's LeftProjection which is a monad */
final class FirstProjection[T1,T2](peer:Pair[T1,T2]) {
	def get:T1	= peer._1
	
	def map[U](func:T1=>U):Pair[U,T2]	= Pair(func(peer._1),	peer._2)
	
	def coFlatMap[U,TT2>:T2](func:Pair[T1,TT2]=>U):Pair[U,TT2]			= Pair(func(peer),	peer._2)
	def coFlatten[U,TT2>:T2](implicit ev:Pair[T1,TT2]=>U):Pair[U,TT2]	= coFlatMap(ev)
	
	def sequenceOption[U](implicit ev:T1=>Option[U]):Option[Pair[U,T2]]		= traverseOption(identity[U])
	def sequenceTried[F,W](implicit ev:T1=>Tried[F,W]):Tried[F,Pair[W,T2]]	= traverseSequence(identity[W])
	
	def traverseOption[U,V](func:U=>V)(implicit ev:T1=>Option[U]):Option[Pair[V,T2]]		= ev(peer._1) map { it => Pair(func(it), peer._2) }
	def traverseSequence[F,W,V](func:W=>V)(implicit ev:T1=>Tried[F,W]):Tried[F,Pair[V,T2]]	= ev(peer._1) map { it => Pair(func(it), peer._2) }
}

/** comonad, see Either's RightProjection which is a monad */
final class SecondProjection[T1,T2](peer:Pair[T1,T2]) {
	def get:T2	= peer._2
	
	def map[U](func:T2=>U):Pair[T1,U]	= Pair(peer._1,	func(peer._2))
	
	def coFlatMap[TT1>:T1,U](func:Pair[TT1,T2]=>U):Pair[TT1,U]			= Pair(peer._1,	func(peer))
	def coFlatten[TT1>:T1,U](implicit ev:Pair[TT1,T2]=>U):Pair[TT1,U]	= coFlatMap(ev)
	
	def sequenceOption[U](implicit ev:T2=>Option[U]):Option[Pair[T1,U]]		= traverseOption(identity[U])
	def sequenceTried[F,W](implicit ev:T2=>Tried[F,W]):Tried[F,Pair[T1,W]]	= traverseSequence(identity[W])
	
	def traverseOption[U,V](func:U=>V)(implicit ev:T2=>Option[U]):Option[Pair[T1,V]]		= ev(peer._2) map { it => Pair(peer._1, func(it)) }
	def traverseSequence[F,W,V](func:W=>V)(implicit ev:T2=>Tried[F,W]):Tried[F,Pair[T1,V]]	= ev(peer._2) map { it => Pair(peer._1, func(it)) }
}
