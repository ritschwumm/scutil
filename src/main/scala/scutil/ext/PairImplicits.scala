package scutil.ext

object PairImplicits extends PairImplicits

trait PairImplicits {
	implicit def toPairExt[T1,T2](delegate:Pair[T1,T2]) = new PairExt[T1,T2](delegate)
}

final class PairExt[T1,T2](delegate:Pair[T1,T2]) {
	def first:FirstProjection[T1,T2]	= new FirstProjection[T1,T2](delegate)
	def second:SecondProjection[T1,T2]	= new SecondProjection[T1,T2](delegate)
}

/** comonad, see Either's LeftProjection which is a monad */
final class FirstProjection[T1,T2](delegate:Pair[T1,T2]) {
	def map[U](func:T1=>U):Pair[U,T2]							= Pair(func(delegate._1), delegate._2)
	def unflatMap[U,TT2>:T2](func:Pair[T1,TT2]=>U):Pair[U,TT2]	= Pair(func(delegate), delegate._2)
}

/** comonad, see Either's RightProjection which is a monad */
final class SecondProjection[T1,T2](delegate:Pair[T1,T2]) {
	def map[U](func:T2=>U):Pair[T1,U]							= Pair(delegate._1, func(delegate._2))
	def unflatMap[TT1>:T1,U](func:Pair[TT1,T2]=>U):Pair[TT1,U]	= Pair(delegate._1, func(delegate))
} 
