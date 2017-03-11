package scutil.lang

object IO {
	def pure[T](it:T):IO[T]		= IO(() => it)
	def delay[T](it: =>T):IO[T]	= IO(() => it)
}

final case class IO[T](run:()=>T) {
	def attempt:IO[Tried[Exception,T]]	=
			IO { () =>
				try { Win(run()) }
				catch { case e:Exception => Fail(e) }
			}
			
	def map[U](func:T=>U):IO[U]	=
			IO { () =>
				func(run())
			}
			
	/** function effect first */
	def pa[U](that:IO[T=>U]):IO[U]	=
			IO { () =>
				that.run()(run())
			}
			
	/** function effect first */
	def ap[U,V](that:IO[U])(implicit ev:T=>U=>V):IO[V]	=
			IO { () =>
				ev(run())(that.run())
			}
			
	def flatMap[U](func:T=>IO[U]):IO[U]	=
			IO { () =>
				func(run()).run()
			}
			
	def flatten[U](implicit ev:T=>IO[U]):IO[U]	=
			flatMap(ev)
		
	def zip[U](that:IO[U]):IO[(T,U)]	=
			for { t	<- this; u	<- that } yield (t,u)
			
	def zipWith[U,V](that:IO[U])(func:(T,U)=>V):IO[V]	=
			for { t	<- this; u	<- that } yield func(t,u)
	
	def first[U](that:IO[U]):IO[T]	=
			for { t	<- this; _	<- that } yield t
		
	def second[U](that:IO[U]):IO[U]	=
			for { _	<- this; u	<- that } yield u
}
