package scutil.pimp

import scutil.lang._

object PairImplicits extends PairImplicits

trait PairImplicits {
	implicit def toPairExt[T1,T2](peer:Pair[T1,T2]) = new PairExt[T1,T2](peer)
}

final class PairExt[T1,T2](peer:Pair[T1,T2]) {
	def first:FirstProjection[T1,T2]	= new FirstProjection[T1,T2](peer)
	def second:SecondProjection[T1,T2]	= new SecondProjection[T1,T2](peer)
	
	def apply1to2[U](implicit ev:T1=>(T2=>U)):U	= ev(peer._1)(peer._2)
	def apply2to1[U](implicit ev:T2=>(T1=>U)):U	= ev(peer._2)(peer._1)
}

/** comonad, see Either's LeftProjection which is a monad */
final class FirstProjection[T1,T2](peer:Pair[T1,T2]) {
	def get:T1	= peer._1
	def map[U](func:T1=>U):Pair[U,T2]								= Pair(func(peer._1),	peer._2)
	def unflatMap[U,TT2>:T2](func:Pair[T1,TT2]=>U):Pair[U,TT2]		= Pair(func(peer),		peer._2)
	def sequenceTried[F,W](ev:T1=>Tried[F,W]):Tried[F,Pair[W,T2]]	= ev(peer._1) map { (_, peer._2) }
}

/** comonad, see Either's RightProjection which is a monad */
final class SecondProjection[T1,T2](peer:Pair[T1,T2]) {
	def get:T2	= peer._2
	def map[U](func:T2=>U):Pair[T1,U]								= Pair(peer._1,	func(peer._2))
	def unflatMap[TT1>:T1,U](func:Pair[TT1,T2]=>U):Pair[TT1,U]		= Pair(peer._1,	func(peer))
	def sequenceTried[F,W](ev:T2=>Tried[F,W]):Tried[F,Pair[T1,W]]	= ev(peer._2) map { (peer._1, _) }
} 
