package scutil.lang.pimp

import scutil.lang._

object PairImplicits extends PairImplicits

trait PairImplicits {
	implicit final class PairExt[T1,T2](peer:Tuple2[T1,T2]) {
		def first:PairFirstProjection[T1,T2]	= new PairFirstProjection[T1,T2](peer)
		def second:PairSecondProjection[T1,T2]	= new PairSecondProjection[T1,T2](peer)

		/** apply function in first element to value in second */
		def applied[U](implicit ev:T1=>(T2=>U)):U		= ev(peer._1)(peer._2)
		/** apply function in second element to value in first */
		def appliedSwap[U](implicit ev:T2=>(T1=>U)):U	= ev(peer._2)(peer._1)

		/** apply binary function to elements */
		def combineWith[U](func:(T1,T2)=>U):U	= func(peer._1, peer._2)

		/** map both elements */
		def bimap[U1,U2](func1:T1=>U1, func2:T2=>U2):(U1,U2)	= (func1(peer._1), func2(peer._2))
	}
}

/** comonad, see Either's LeftProjection which is a monad */
final class PairFirstProjection[T1,T2](peer:Tuple2[T1,T2]) {
	def get:T1	= peer._1

	def map[U](func:T1=>U):Tuple2[U,T2]	= Tuple2(func(peer._1),	peer._2)

	def coFlatMap[U,TT2>:T2](func:Tuple2[T1,TT2]=>U):Tuple2[U,TT2]			= Tuple2(func(peer),	peer._2)
	def coFlatten[U,TT2>:T2](implicit ev:Tuple2[T1,TT2]=>U):Tuple2[U,TT2]	= coFlatMap(ev)

	def sequenceOption[U](implicit ev:T1=>Option[U]):Option[Tuple2[U,T2]]					= traverseOption(ev)
	def sequenceEither[F,W](implicit ev:T1=>Either[F,W]):Either[F,Tuple2[W,T2]]				= traverseEither(ev)
	def sequenceValidated[F,W](implicit ev:T1=>Validated[F,W]):Validated[F,Tuple2[W,T2]]	= traverseValidated(ev)

	def traverseOption[U](func:T1=>Option[U]):Option[Tuple2[U,T2]]					= func(peer._1) map { Tuple2(_, peer._2) }
	def traverseEither[F,W](func:T1=>Either[F,W]):Either[F,Tuple2[W,T2]]			= func(peer._1) map { Tuple2(_, peer._2) }
	def traverseValidated[F,W](func:T1=>Validated[F,W]):Validated[F,Tuple2[W,T2]]	= func(peer._1) map { Tuple2(_, peer._2) }
}

/** comonad, see Either's RightProjection which is a monad */
final class PairSecondProjection[T1,T2](peer:Tuple2[T1,T2]) {
	def get:T2	= peer._2

	def map[U](func:T2=>U):Tuple2[T1,U]	= Tuple2(peer._1,	func(peer._2))

	def coFlatMap[TT1>:T1,U](func:Tuple2[TT1,T2]=>U):Tuple2[TT1,U]			= Tuple2(peer._1,	func(peer))
	def coFlatten[TT1>:T1,U](implicit ev:Tuple2[TT1,T2]=>U):Tuple2[TT1,U]	= coFlatMap(ev)

	def sequenceOption[U](implicit ev:T2=>Option[U]):Option[Tuple2[T1,U]]					= traverseOption(ev)
	def sequenceEither[F,W](implicit ev:T2=>Either[F,W]):Either[F,Tuple2[T1,W]]				= traverseEither(ev)
	def sequenceValidated[F,W](implicit ev:T2=>Validated[F,W]):Validated[F,Tuple2[T1,W]]	= traverseValidated(ev)

	def traverseOption[U](func:T2=>Option[U]):Option[Tuple2[T1,U]]					= func(peer._2) map { Tuple2(peer._1, _) }
	def traverseEither[F,W](func:T2=>Either[F,W]):Either[F,Tuple2[T1,W]]			= func(peer._2) map { Tuple2(peer._1, _) }
	def traverseValidated[F,W](func:T2=>Validated[F,W]):Validated[F,Tuple2[T1,W]]	= func(peer._2) map { Tuple2(peer._1, _) }
}
