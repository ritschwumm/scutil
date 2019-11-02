package scutil.lang

import scala.annotation.tailrec

import scutil.lang.tc._

object Io extends IoInstances {
	def pure[T](it:T):Io[T]			= Io.Pure(it)
	def delay[T](it: =>T):Io[T]		= Io.Suspend(() => it)
	def thunk[T](it:Thunk[T]):Io[T]	= Io.Suspend(it)

	def pureUnit:Io[Unit]	= Io.Pure(())

	//------------------------------------------------------------------------------

	// TODO how about cokleisli?

	def staticToKleisli[S,T](func:Io[S=>T]):(S => Io[T])	=
			s	=>  delay { func unsafeRun () apply s }

	def kleisliToStatic[S,T](func:S=>Io[T]):Io[S=>T]	=
			delay { s => func(s) unsafeRun () }

	//------------------------------------------------------------------------------

	val ToLater:NaturalTransformation[Io,Later]	=
			new NaturalTransformation[Io,Later] {
				def apply[T](orig:Io[T]):Later[T]	= orig.toLater
			}

	//------------------------------------------------------------------------------

	final case class Pure[T](value:T) 							extends Io[T]
	final case class Suspend[T](thunk:()=>T) 					extends Io[T]
	final case class Map[S,T](base:Io[S], func:S=>T) 			extends Io[T]
	final case class FlatMap[S,T](base:Io[S], func:S=>Io[T]) 	extends Io[T]
}

sealed trait Io[T] {
	@tailrec
	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	final def unsafeRun():T	=
			this match {
				case Io.Pure(value)		=> value
				case Io.Suspend(thunk)	=> thunk()
				case Io.Map(base2, func2)	=>
					base2 match {
						case Io.Pure(value)				=> func2(value)
						case Io.Suspend(thunk)			=> func2(thunk())
						case Io.Map(base1, func1)		=> Io.Map(base1, func1 andThen func2).asInstanceOf[Io[T]].unsafeRun()
						case Io.FlatMap(base1, func1)	=> Io.FlatMap(base1, (it:Any) => Io.Map(func1(it), func2)).asInstanceOf[Io[T]].unsafeRun()
					}
				case Io.FlatMap(base2, func2)	=>
					base2 match {
						case Io.Pure(value)				=> func2(value).unsafeRun()
						case Io.Suspend(thunk)			=> func2(thunk()).unsafeRun()
						case Io.Map(base1, func1)		=> Io.FlatMap(base1, func1 andThen func2).asInstanceOf[Io[T]].unsafeRun()
						case Io.FlatMap(base1, func1)	=> Io.FlatMap(base1, (it:Any) => Io.FlatMap(func1(it), func2)).asInstanceOf[Io[T]].unsafeRun()
					}
			}

	final def attempt:Io[Either[Exception,T]]	=
			Io.Suspend { () =>
				try { Right(unsafeRun()) }
				catch { case e:Exception => Left(e) }
			}

	final def map[U](func:T=>U):Io[U]	=
			Io.Map(this, func)

	/** function effect first */
	final def pa[U](that:Io[T=>U]):Io[U]	=
			Io.FlatMap(that, (tu:(T=>U)) => Io.Map(this, (t:T) => tu(t)))
			//for { f <- that; v <- this } yield f(v)

	/** function effect first */
	final def ap[U,V](that:Io[U])(implicit ev:T=>U=>V):Io[V]	=
			that pa (this map ev)

	final def flatMap[U](func:T=>Io[U]):Io[U]	=
			Io.FlatMap(this, func)

	final def flatten[U](implicit ev:T=>Io[U]):Io[U]	=
			flatMap(ev)

	final def zip[U](that:Io[U]):Io[(T,U)]	=
			Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => (t,u)))
			//for { t	<- this; u	<- that } yield (t,u)

	final def zipWith[U,V](that:Io[U])(func:(T,U)=>V):Io[V]	=
			Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => func(t,u)))
			//for { t	<- this; u	<- that } yield func(t,u)

	final def first[U](that:Io[U]):Io[T]	=
			Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => t))
			//for { t	<- this; _	<- that } yield t

	final def second[U](that:Io[U]):Io[U]	=
			Io.FlatMap(this, (t:T) => Io.Map(that, (u:U) => u))
			//for { _	<- this; u	<- that } yield u

	final def toLater:Later[T]	=
			Later { cont =>
				cont(unsafeRun())
			}
}

trait IoInstances {
	implicit val IoUnitMonoid:Monoid[Io[Unit]]	=
			Monoid instance (
				Io.pureUnit,
				(a, b) =>
							 if (a == Io.pureUnit)	b
						else if (b == Io.pureUnit)	a
						else						a second b
			)

	implicit val IoMonad:Monad[Io]	=
			new Monad[Io] {
				override def pure[A](it:A):Io[A]							= Io pure it
				override def map[A,B](it:Io[A])(func:A=>B):Io[B]			= it map func
				override def flatMap[A,B](it:Io[A])(func:A=>Io[B]):Io[B]	= it flatMap func
			}

	implicit val IoDelay:Delay[Io]	=
			new Delay[Io] {
				override def delay[T](it: =>T):Io[T]	= Io delay it
			}
}
