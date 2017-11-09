package scutil.lang

import scutil.lang.tc._

object Io extends IoInstances {
	def pure[T](it:T):Io[T]			= Io(() => it)
	def delay[T](it: =>T):Io[T]		= Io(() => it)
	def thunk[T](it:Thunk[T]):Io[T]	= Io(it)
	
	//------------------------------------------------------------------------------
	
	def newIoRef[T](initial: =>T):Io[IoRef[T]]	=
			Io delay IoRef(initial)
	
	//------------------------------------------------------------------------------
	
	// TODO how about cokleisli?
	
	def staticToKleisli[S,T](func:Io[S=>T]):(S => Io[T])	=
			s	=>  delay { func unsafeRun () apply s }
		
	def kleisliToStatic[S,T](func:S=>Io[T]):Io[S=>T]	=
			delay { s => func(s) unsafeRun () }
}

final case class Io[T](unsafeRun:()=>T) {
	def attempt:Io[Either[Exception,T]]	=
			Io { () =>
				try { Right(unsafeRun()) }
				catch { case e:Exception => Left(e) }
			}
			
	def map[U](func:T=>U):Io[U]	=
			Io { () =>
				func(unsafeRun())
			}
			
	/** function effect first */
	def pa[U](that:Io[T=>U]):Io[U]	=
			Io { () =>
				that.unsafeRun()(unsafeRun())
			}
			
	/** function effect first */
	def ap[U,V](that:Io[U])(implicit ev:T=>U=>V):Io[V]	=
			that pa (this map ev)
			
	def flatMap[U](func:T=>Io[U]):Io[U]	=
			Io { () =>
				func(unsafeRun()).unsafeRun()
			}
			
	def flatten[U](implicit ev:T=>Io[U]):Io[U]	=
			flatMap(ev)
		
	def zip[U](that:Io[U]):Io[(T,U)]	=
			for { t	<- this; u	<- that } yield (t,u)
			
	def zipWith[U,V](that:Io[U])(func:(T,U)=>V):Io[V]	=
			for { t	<- this; u	<- that } yield func(t,u)
	
	def first[U](that:Io[U]):Io[T]	=
			for { t	<- this; _	<- that } yield t
		
	def second[U](that:Io[U]):Io[U]	=
			for { _	<- this; u	<- that } yield u
		
	def toLater:Later[T]	=
			Later { cont =>
				cont(unsafeRun())
			}
}

trait IoInstances {
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
