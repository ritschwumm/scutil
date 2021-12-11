package scutil.lang.extension

import java.util.{ Optional => JOptional }

import scala.collection.Factory
import scala.collection.generic.IsIterable

import scutil.lang._
import scutil.lang.tc._

object OptionImplicits extends OptionImplicits

trait OptionImplicits {
	implicit final class OptionCompanionImplicits(peer:Option.type) {
		def none[T]:Option[T]		= None
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

		/*
		// NOTE we get these from applicative syntax

		// * ap of the monad, <*> of the applicative functor
		def ap[U,V](source:Option[U])(implicit ev: T <:< (U=>V)):Option[V] =
			for { f	<- peer; s	<- source } yield f(s)

		def product[U](that:Option[U]):Option[(T,U)]	=
			peer zip that

		def map2[U,V](that:Option[U])(func:(T,U)=>V):Option[V]	=
			(peer,that) match {
				case ((Some(t),Some(u)))	=> Some(func(t,u))
				case _						=> None
			}
		*/

		/** the partition method defined on Iterable is useless */
		def partition(pred:Predicate[T]):(Option[T],Option[T])	=
			(peer filter pred, peer filterNot pred)

		def partitionEither[U,V](implicit ev: T <:< Either[U,V]):(Option[U],Option[V])	=
			peer map ev match {
				case Some(Left(x))	=> (Some(x),	None)
				case Some(Right(x))	=> (None,		Some(x))
				case None			=> (None,		None)
			}

		@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
		def flatMapMany[Repr](func:T=>Repr)(implicit iter:IsIterable[Repr], factory:Factory[iter.A,Repr]):Repr	=
			peer.map(func).flattenMany

		@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
		def flattenMany(implicit iter:IsIterable[T], factory:Factory[iter.A,T]):T	=
			peer.getOrElse(factory.newBuilder.result())

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

		def sequenceOption[U](implicit ev: T <:< Option[U]):Option[Option[U]]	=
			traverseOption(ev)

		def traverseOption[U](func:T=>Option[U]):Option[Option[U]]	=
			peer map func match {
				case None		=> Some(None)
				case Some(None)	=> None
				case x			=> x
			}

		// TODO generify to any Iterable

		def sequenceSeq[U](implicit ev: T <:< Seq[U]):Seq[Option[U]]	=
			traverseSeq(ev)

		def traverseSeq[U](func:T=>Seq[U]):Seq[Option[U]]	=
			peer map func match {
				case None		=> Seq(None)
				case Some(xs)	=> xs map Some.apply
			}

		/*
		// TODO generify these - right now, they are not working

		def sequenceIterable[CC[_]<:Iterable[U],U](implicit ev: T <:< CC[U], factory:Factory[Option[U],CC[Option[U]]]):CC[Option[U]]	=
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

		def sequenceEither[F,W](implicit ev: T <:< Either[F,W]):Either[F,Option[W]]	=
			traverseEither(ev)

		def traverseEither[F,W](func:T=>Either[F,W]):Either[F,Option[W]]	=
			peer map func match {
				case None			=> Right(None)
				case Some(Left(x))	=> Left(x)
				case Some(Right(x))	=> Right(Some(x))
			}

		def sequenceValidated[F,W](implicit ev: T <:< Validated[F,W]):Validated[F,Option[W]]	=
			traverseValidated(ev)

		def traverseValidated[F,W](func:T=>Validated[F,W]):Validated[F,Option[W]]	=
			peer map func match {
				case None						=> Validated.valid(None)
				case Some(Validated.Invalid(x))	=> Validated.invalid(x)
				case Some(Validated.Valid(x))	=> Validated.valid(Some(x))
			}

		def sequenceState[S,U](implicit ev:T <:< State[S,U]):State[S,Option[U]]	=
			traverseState(ev)

		def traverseState[S,U](func:T=>State[S,U]):State[S,Option[U]]	=
			peer map func match {
				case None		=> State pure None
				case Some(st)	=> st map Some.apply
			}

		// TODO state support StateT

		//------------------------------------------------------------------------------

		def oneOrTwo(that:Option[T])(combine:(T,T)=>T):Option[T]	=
			(peer, that) match {
				case (None,		None)		=> None
				case (Some(aa),	None)		=> Some(aa)
				case (None,		Some(bb))	=> Some(bb)
				case (Some(aa),	Some(bb))	=> Some(combine(aa, bb))
			}

		//------------------------------------------------------------------------------

		def failEither:Either[T,Unit]	=
			peer.toLeft(())

		def failValidated:Validated[T,Unit]	=
			toInvalid(())

		//------------------------------------------------------------------------------

		// exists: toLeft
		// exists: toRight

		def toValid[F](invalid: =>F):Validated[F,T]	=
			peer match {
				case None		=> Validated.invalid(invalid)
				case Some(x)	=> Validated.valid(x)
			}

		def toInvalid[ES,W](valid: =>W):Validated[T,W]	=
			peer match {
				case None		=> Validated.valid(valid)
				case Some(x)	=> Validated.invalid(x)
			}

		def toValidNes[F](invalid: =>F):Validated[Nes[F],T]	=
			peer match {
				case None		=> Validated.invalid(Nes.one(invalid))
				case Some(x)	=> Validated.valid(x)
			}

		// NOTE toSeq there from Iterable, but we don't want the implicit conversion
		def toSeq:Seq[T]	=
			peer.toList

		// NOTE toSet there from Iterable, but we don't want the implicit conversion
		def toSet:Set[T]	=
			peer match {
				case Some(x)	=> Set(x)
				case None		=> Set.empty
			}

		// NOTE toVector there from Iterable, but we don't want the implicit conversion
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

		def toRightF[F[_]:Monad,L](leftValue: =>F[L]):EitherT[F,L,T]	=
			toOptionT[F] toRightF leftValue

		def toRightT[F[_]:Applicative,L](leftValue: =>L):EitherT[F,L,T]	=
			toOptionT[F] toRight leftValue

		def toLeftF[F[_]:Monad,R](rightValue: =>F[R]):EitherT[F,T,R]	=
			toOptionT[F] toLeftF rightValue

		def toLeftT[F[_]:Applicative,R](rightValue: =>R):EitherT[F,T,R]	=
			toOptionT[F] toLeft rightValue
	}
}
