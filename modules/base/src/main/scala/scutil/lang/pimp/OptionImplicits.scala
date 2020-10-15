package scutil.lang.pimp

import java.util.{ Optional => JOptional }

import scala.collection.Factory

import scutil.lang._
import scutil.lang.tc._

object OptionImplicits extends OptionImplicits

trait OptionImplicits {
	implicit final class OptionCompanionImplicits(peer:Option.type) {
		def none[T]:Option[T]	= None
		def some[T](it:T):Option[T]	= Some(it)

		def switch[T](condition:Boolean, trueSome: =>T):Option[T]	=
			if (condition)	Some(trueSome)
			else			None
	}

	implicit final class OptionExt[T](peer:Option[T]) {
		def getOrError(s:String)	= peer getOrElse (sys error s)

		def cata[X](none: => X, some:T => X):X =
			peer match {
				case Some(x)	=> some(x)
				case None		=> none
			}

		/** ap of the monad, <*> of the applicative functor */
		def ap[U,V](source:Option[U])(implicit ev:T=>U=>V):Option[V] =
			for { f	<- peer; s	<- source } yield f(s)

		/** ap with inverted parameters, aka <**> */
		def pa[U](func:Option[T=>U]):Option[U] =
			for { f	<- func; s	<- peer } yield f(s)

		/** the partition method defined on Iterable is useless */
		def partition(pred:Predicate[T]):(Option[T],Option[T])	=
			(peer filter pred, peer filterNot pred)

		def zipBy[U](func:T=>U):Option[(T,U)]	=
			peer map { it => (it,func(it)) }

		def zipWith[U,V](that:Option[U])(func:(T,U)=>V):Option[V]	=
			(peer,that) match {
				case ((Some(t),Some(u)))	=> Some(func(t,u))
				case _						=> None
			}

		def partitionEither[U,V](implicit ev:T=>Either[U,V]):(Option[U],Option[V])	=
			peer map ev match {
				case Some(Left(x))	=> (Some(x),	None)
				case Some(Right(x))	=> (None,		Some(x))
				case None			=> (None,		None)
			}

		/** handy replacement for opt.toSeq flatMap func abusing Factory as a Zero typeclass */
		def flatMapMany[U,CC[_]](func:T=>CC[U])(implicit factory:Factory[U,CC[U]]):CC[U]	=
			peer map func match {
				case Some(cc)	=> cc
				case None		=> factory.newBuilder.result()
			}

		/** handy replacement for opt.toSeq.flatten abusing Factory as a Zero typeclass */
		def flattenMany[U,CC[_]](implicit ev:T=>CC[U], factory:Factory[U,CC[U]]):CC[U]	=
			flatMapMany(ev)

		//------------------------------------------------------------------------------

		def someEffect(effect:T=>Unit):Option[T] = {
			peer foreach effect
			peer
		}

		def noneEffect(effect: =>Unit):Option[T] = {
			if (peer.isEmpty)  effect
			peer
		}

		//------------------------------------------------------------------------------

		def sequenceOption[U](implicit ev:PFunction[T,U]):Option[Option[U]]	=
			traverseOption(ev)

		def traverseOption[U](func:PFunction[T,U]):Option[Option[U]]	=
			peer map func match {
				case None		=> Some(None)
				case Some(None)	=> None
				case x			=> x
			}

		// TODO generify to any Iterable

		def sequenceSeq[U](implicit ev:T=>Seq[U]):Seq[Option[U]]	=
			traverseSeq(ev)

		def traverseSeq[U](func:T=>Seq[U]):Seq[Option[U]]	=
			peer map func match {
				case None		=> Seq(None)
				case Some(xs)	=> xs map Some.apply
			}

		/*
		// TODO generify these - right now, they are not working

		def sequenceIterable[CC[_]<:Iterable[U],U](implicit ev:T=>CC[U], factory:Factory[Option[U],CC[Option[U]]]):CC[Option[U]]	=
				traverseIterable(ev)

		def traverseIterable[CC[_]<:Iterable[U],U](func:T=>CC[U])(implicit factory:Factory[Option[U],CC[Option[U]]]):CC[Option[U]]	= {
			val builder	= factory.newBuilder
			peer map func match {
				case None		=> builder += None; builder.result
				case Some(xs)	=> xs foreach { x => builder += Some(x) }
			}
			builder.result
		}
		*/

		def sequenceEither[F,W](implicit ev:T=>Either[F,W]):Either[F,Option[W]]	=
			traverseEither(ev)

		def traverseEither[F,W](func:T=>Either[F,W]):Either[F,Option[W]]	=
			peer map func match {
				case None			=> Right(None)
				case Some(Left(x))	=> Left(x)
				case Some(Right(x))	=> Right(Some(x))
			}

		def sequenceValidated[F,W](implicit ev:T=>Validated[F,W]):Validated[F,Option[W]]	=
			traverseValidated(ev)

		def traverseValidated[F,W](func:T=>Validated[F,W]):Validated[F,Option[W]]	=
			peer map func match {
				case None			=> Good(None)
				case Some(Bad(x))	=> Bad(x)
				case Some(Good(x))	=> Good(Some(x))
			}

		def sequenceState[S,U](implicit ev:T=>State[S,U]):State[S,Option[U]]	=
			traverseState(ev)

		def traverseState[S,U](func:T=>State[S,U]):State[S,Option[U]]	=
			peer map func match {
				case None		=> State pure None
				case Some(st)	=> st map Some.apply
			}

		// TODO state support StateT

		//------------------------------------------------------------------------------

		def oneOrTwo(that:Option[T])(concat:(T,T)=>T):Option[T]	=
			(peer, that) match {
				case (None,		None)		=> None
				case (Some(aa),	None)		=> Some(aa)
				case (None,		Some(bb))	=> Some(bb)
				case (Some(aa),	Some(bb))	=> Some(concat(aa, bb))
			}

		//------------------------------------------------------------------------------

		def failEither:Either[T,Unit]	=
			toLeft(())

		def failValidated:Validated[T,Unit]	=
			toBad(())

		//------------------------------------------------------------------------------

		def toRight[F](leftValue: =>F):Either[F,T]	=
			peer match {
				case None		=> Left(leftValue)
				case Some(x)	=> Right(x)
			}

		def toLeft[W](rightValue: =>W):Either[T,W]	=
			peer match {
				case None		=> Right(rightValue)
				case Some(x)	=> Left(x)
			}

		def toGood[F](problems: =>F):Validated[F,T]	=
			peer match {
				case None		=> Bad(problems)
				case Some(x)	=> Good(x)
			}

		def toBad[ES,W](good: =>W):Validated[T,W]	=
			peer match {
				case None		=> Good(good)
				case Some(x)	=> Bad(x)
			}

		def toSeq:Seq[T]	=
			toVector

		def toSet:Set[T]	=
			toVector.toSet

		def toVector:Vector[T]	=
			peer match {
				case Some(x)	=> Vector(x)
				case None		=> Vector.empty
			}

		def toJOptional:JOptional[T]	=
			peer match {
				case Some(x)	=> JOptional of x
				case None		=> JOptional.empty[T]
			}

		//------------------------------------------------------------------------------

		def toOptionT[F[_]:Applicative]:OptionT[F,T]	=
			OptionT fromOption peer

		def toRightT[F[_]:Monad,L](leftValue: =>F[L]):EitherT[F,L,T]	=
			toOptionT[F] toRight leftValue

		def toRightPureT[F[_]:Applicative,L](leftValue: =>L):EitherT[F,L,T]	=
			toOptionT[F] toRightPure leftValue

		def toLeftT[F[_]:Monad,R](rightValue: =>F[R]):EitherT[F,T,R]	=
			toOptionT[F] toLeft rightValue

		def toLeftPureT[F[_]:Applicative,R](rightValue: =>R):EitherT[F,T,R]	=
			toOptionT[F] toLeftPure rightValue
	}
}
