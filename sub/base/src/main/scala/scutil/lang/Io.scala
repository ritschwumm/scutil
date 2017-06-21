package scutil.lang

import scutil.lang.tc._

object Io extends IoInstances {
	def pure[T](it:T):Io[T]					= Io(() => it)
	def delay[T](it: =>T):Io[T]				= Io(() => it)
	def delayThunk[T](it:Thunk[T]):Io[T]	= Io(it)
}

final case class Io[T](run:()=>T) {
	def attempt:Io[Either[Exception,T]]	=
			Io { () =>
				try { Right(run()) }
				catch { case e:Exception => Left(e) }
			}
			
	def map[U](func:T=>U):Io[U]	=
			Io { () =>
				func(run())
			}
			
	/** function effect first */
	def pa[U](that:Io[T=>U]):Io[U]	=
			Io { () =>
				that.run()(run())
			}
			
	/** function effect first */
	def ap[U,V](that:Io[U])(implicit ev:T=>U=>V):Io[V]	=
			that pa (this map ev)
			
	def flatMap[U](func:T=>Io[U]):Io[U]	=
			Io { () =>
				func(run()).run()
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
				override def delay[T](it: =>T):Io[T]			= Io delay it
				override def delayThunk[T](it:Thunk[T]):Io[T]	= Io delayThunk it
			}
}
