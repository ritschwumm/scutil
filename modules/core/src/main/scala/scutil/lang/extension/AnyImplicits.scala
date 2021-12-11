package scutil.lang.extension

import scala.annotation.tailrec
import scala.util.{ Try, Success, Failure }
import scala.concurrent.Future
import scala.collection.IterableFactory

import scutil.lang._

object AnyImplicits extends AnyImplicits

trait AnyImplicits {
	implicit final class AnyExt[T](peer:T) {
		/** ensure we get the java wrapper */
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		def boxed:AnyRef	= peer.asInstanceOf[AnyRef]

		/** type-invariant equality */
		def ====[U](that:U)(implicit ev:T=:=U):Boolean	= ev(peer) == that

		/** type-invariant inequality */
		def !===[U](that:T)(implicit ev:T=:=U):Boolean	= ev(peer) != that

		//------------------------------------------------------------------------------

		// TODO these are in scala.util.chaining now: into is named pipe, and doto is named tap

		/** symbolic alias for into */
		def |>[U](f:T=>U):U	= into(f)

		/** apply a single unary function, just like F#'s operator or ruby's thrush */
		def into[U](f:T=>U):U	= f(peer)

		/** apply an effect, then return the peer itself; inspired by clojure's doto */
		def doto(effect:T=>Unit):T	= {
			effect apply peer
			peer
		}

		//------------------------------------------------------------------------------

		/** match lifted to an Option */
		def matchOption[U](pf:PartialFunction[T,U]):Option[U] =
			PartialFunction.condOpt(peer)(pf)

		def flatMatchOption[U](pf:PartialFunction[T,Option[U]]):Option[U] =
			PartialFunction.condOpt(peer)(pf).flatten

		def matchBoolean[U](pf:PartialFunction[T,Unit]):Boolean =
			pf isDefinedAt peer

		//------------------------------------------------------------------------------

		def some:Option[T]			= Some(peer)

		def asRight[L]:Either[L,T]		= Right(peer)
		def asLeft[R]:Either[T,R]		= Left(peer)

		def valid[L]:Validated[L,T]		= Validated.valid(peer)
		def invalid[R]:Validated[T,R]	= Validated.invalid(peer)

		def iorLeft[B]:Ior[T,B]			= Ior.left(peer)
		def iorRight[A]:Ior[A,T]		= Ior.right(peer)
		def iorBoth:Ior[T,T]			= Ior.both(peer, peer)

		def inNes:Nes[T]				= Nes.one(peer)

		def inVector:Vector[T]			= Vector(peer)
		def inList:List[T]				= List(peer)

		def inSuccess:Try[T]									= Success(peer)
		def inFailure[X](implicit ev: T <:< Throwable):Try[X]	= Failure(peer)

		def inFuture:Future[T]			= Future successful peer

		def one[CC[_]](factory:IterableFactory[CC]):CC[T]	= factory(peer)

		//------------------------------------------------------------------------------

		/** Some if the predicate matches, else None */
		def optionBy(predicate:T=>Boolean):Option[T]	=
			if (predicate(peer)) Some(peer) else None

		/** None if the predicate matches, else Some */
		def optionNotBy(predicate:T=>Boolean):Option[T]	=
			if (predicate(peer)) None else Some(peer)

		/** Right if the predicate matches, else Left */
		def eitherBy(predicate:T=>Boolean):Either[T,T]	=
			if (predicate(peer)) Right(peer) else Left(peer)

		/** Right if Some else original value in Left */
		def rightBy[U](func:T=>Option[U]):Either[T,U]	=
			func(peer) toRight peer

		/** Left if Some else original value in Right */
		def leftBy[U](func:T=>Option[U]):Either[U,T]	=
			func(peer) toLeft peer

		/** Fail if Some else original value in Win */
		def invalidBy[E](func:T=>Option[E]):Validated[E,T]	=
			func(peer) match {
				case Some(x)	=> Validated.invalid(x)
				case None		=> Validated.valid(peer)
			}

		//------------------------------------------------------------------------------

		// TODO clean these up

		/** pair with function applied */
		def firstBy[U](func:T=>U):(T,U)	=
			(peer, func(peer))

		/** pair with function applied */
		def secondBy[U](func:T=>U):(U,T)	=
			(func(peer), peer)

		/** pair with another value */
		def firstWith[U](that:U):(T,U)	=
			(peer, that)

		/** pair with function applied */
		def secondWith[U](that:U):(U,T)	=
			(that, peer)

		/** pair with itself */
		def duplicate:(T,T)	= (peer, peer)

		//------------------------------------------------------------------------------

		/** apply as long as we get another Left */
		def tailRec[U](func:T=>Either[T,U]):U	= {
			@tailrec
			def loop(it:T):U	=
				func(it) match {
					case Left(x)	=> loop(x)
					case Right(x)	=> x
				}
			loop(peer)
		}

		/** apply func and check after each step whether to continue (Left) or finish (Right) */
		def tailFold[U,V](func:T=>U)(next:(T,U)=>Either[T,V]):V	= {
			@tailrec
			def loop(it:T):V	= {
				val tmp	= func(it)
				next(it, tmp) match {
					case Left(x)	=> loop(x)
					case Right(x)	=> x
				}
			}
			loop(peer)
		}

		/** apply until the value doesn't change any more */
		def fixpointEquals(func:T=>T):T	=
			fixpoint(func, _ == _)

		/** apply until the value doesn't change any more */
		def fixpoint(func:T=>T, equal:(T,T)=>Boolean):T	=
			tailFold[T,T](func) { (a,b) =>
				if (equal(a,b))	Right(a)
				else			Left(b)
			}

		/** apply until the value becomes None */
		def rewrite(func:T=>Option[T]):T	=
			tailFold(func) { (t, tOpt) =>
				tOpt toLeft t
			}
	}
}
