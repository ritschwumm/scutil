package scutil.lang

import scala.annotation.tailrec

import scutil.lang.tc.*
import scutil.time.MilliDuration

object Io extends IoInstancesLow {
	def pure[T](it:T):Io[T]				= Pure(it)
	def raise[T](error:Exception):Io[T]	= Raise(error)
	def delay[T](it: =>T):Io[T]			= Suspend(() => it)
	def thunk[T](it:()=>T):Io[T]		= Suspend(it)

	//------------------------------------------------------------------------------

	def unit:Io[Unit]	= Pure(())

	def fromEither[T](value:Either[Exception,T]):Io[T]	=
		value match {
			case Left(e)	=> Raise(e)
			case Right(t)	=> Pure(t)
		}

	// TODO have a node for this?
	def suspend[T](it: =>Io[T]):Io[T]	= delay(it).flatten

	def raiseWithSecondary[T](primary:Exception, secondary:Exception):Io[T]	=
		delay{ primary.addSuppressed(secondary) }.productR(raise(primary))

	//------------------------------------------------------------------------------

	def forever(action:Io[Unit]):Io[Unit]	=
		delay {
			while (true) {
				action.unsafeRun()
			}
		}

	def loopWhile(action:Io[Boolean]):Io[Unit]	=
		delay {
			while (action.unsafeRun()) {}
		}

	def loopTo[T](action:Io[Option[T]]):Io[T]	= {
		delay {
			@tailrec
			def loop():T	= {
				action.unsafeRun() match {
					case Some(x)	=> x
					case None		=> loop()
				}
			}
			loop()
		}
	}

	def sleep(duration:MilliDuration):Io[Unit]	=
		delay { Thread.sleep(duration.millis) }

	//------------------------------------------------------------------------------

	// NOTE cokleisli would be unsafe by definition

	def staticToKleisli[S,T](func:Io[S=>T]):(S => Io[T])	=
		s	=>  delay { func.unsafeRun().apply(s) }

	def kleisliToStatic[S,T](func:S=>Io[T]):Io[S=>T]	=
		delay { s => func(s).unsafeRun() }

	//------------------------------------------------------------------------------

	val ToResponder:NaturalTransformation[Io,Responder]	=
		new NaturalTransformation[Io,Responder] {
			def apply[T](orig:Io[T]):Responder[T]	= orig.toResponder
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	given IoMonoid[T](using F:Monoid[T]):Monoid[Io[T]]	=
		new Monoid[Io[T]] {
			def empty:Io[T]						= Io.pure(F.empty)
			def combine(a:Io[T], b:Io[T]):Io[T]	= a.map2(b)(F.combine)
		}

	given IoMonad:Monad[Io]	=
		new Monad[Io] {
			override def pure[A](it:A):Io[A]							= Io.pure(it)
			override def map[A,B](it:Io[A])(func:A=>B):Io[B]			= it.map(func)
			override def flatMap[A,B](it:Io[A])(func:A=>Io[B]):Io[B]	= it.flatMap(func)
		}

	given IoDelay:Delay[Io]	=
		new Delay[Io] {
			override def delay[T](it: =>T):Io[T]	= Io.delay(it)
		}

	//------------------------------------------------------------------------------

	@tailrec
	private def unsafeRun[T](self:Io[T]):T	=
		self match {
			case Io.Pure(value)		=> value
			case Io.Raise(error)	=> throw error
			case Io.Suspend(thunk)	=> thunk()
			case Io.Map(base2, func2)		=>
				base2 match {
					case Io.Pure(value)				=> func2(value)
					case Io.Raise(error)			=> throw error
					case Io.Suspend(thunk)			=> func2(thunk())
					case Io.Map(base1, func1)		=> unsafeRun(Io.Map(base1, func1.andThen(func2)))
					case Io.FlatMap(base1, func1)	=> unsafeRun(Io.FlatMap(base1, it => Io.Map(func1(it), func2)))
				}
			case Io.FlatMap(base2, func2)	=>
				base2 match {
					case Io.Pure(value)				=> unsafeRun(func2(value))
					case Io.Raise(error)			=> throw error
					case Io.Suspend(thunk)			=> unsafeRun(func2(thunk()))
					case Io.Map(base1, func1)		=> unsafeRun(Io.FlatMap(base1, func1.andThen(func2)))
					case Io.FlatMap(base1, func1)	=> unsafeRun(Io.FlatMap(base1, it => Io.FlatMap(func1(it), func2)))
				}
		}
}

enum Io[T] {
	case Pure[T](value:T)							extends Io[T]
	case Raise[T](error:Exception)					extends Io[T]
	case Suspend[T](thunk:()=>T)					extends Io[T]
	case Map[S,T](base:Io[S], func:S=>T)			extends Io[T]
	case FlatMap[S,T](base:Io[S], func:S=>Io[T])	extends Io[T]

	final def unsafeRun():T	= Io.unsafeRun(this)

	final def map[U](func:T=>U):Io[U]	=
		Io.Map(this, func)

	/** function effect first */
	final def ap[U,V](that:Io[U])(using ev:T <:< (U=>V)):Io[V]	=
		Io.FlatMap(this, (f:T) => Io.Map(that, (u:U) => f(u)))

	final def flatMap[U](func:T=>Io[U]):Io[U]	=
		Io.FlatMap(this, func)

	final def flatten[U](using ev:T <:< Io[U]):Io[U]	=
		Io.FlatMap(this, ev)

	final def product[U](that:Io[U]):Io[(T,U)]	=
		Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => (t,u)))

	final def map2[U,V](that:Io[U])(func:(T,U)=>V):Io[V]	=
		Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => func(t,u)))

	final def productL[U](that:Io[U]):Io[T]	=
		Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => t))

	final def productR[U](that:Io[U]):Io[U]	=
		Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => u))

	final def attempt:Io[Either[Exception,T]]	=
		Io.Suspend { () =>
			try { Right(unsafeRun()) }
			catch { case e:Exception => Left(e) }
		}

	final def rethrow[U](using ev:T <:< Either[Exception,U]):Io[U]	=
		Io.FlatMap(this, (it:T) => Io.fromEither(ev(it)))

	def guarantee(cleanup:Io[Unit]):Io[T]	=
		for {
			thisEither	<-	this.attempt
			thatEither	<-	cleanup.attempt
			result		<-	(thisEither, thatEither) match {
								case (Left(thisError),	Left(thatError))	=> Io.raiseWithSecondary(thisError, thatError)
								case (Left(thisError),	_)					=> Io.raise(thisError)
								case (_,				Left(thatError))	=> Io.raise(thatError)
								case (Right(thisOut),	_)					=> Io.pure(thisOut)
						}
		}
		yield result

	final def toResponder:Responder[T]	=
		Responder { cont =>
			cont(unsafeRun())
		}
}

trait IoInstancesLow {
	/** this exists for cases where we only have a Semigroup for T and not a full Monoid */
	given IoSemigroup[T](using F:Semigroup[T]):Semigroup[Io[T]]	=
		new Semigroup[Io[T]] {
			def combine(a:Io[T], b:Io[T]):Io[T]	= a.map2(b)(F.combine)
		}
}
