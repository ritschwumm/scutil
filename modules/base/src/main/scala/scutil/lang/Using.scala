package scutil.lang

import scutil.lang.tc._

// TODO using alternative: Io[T,Io[Unit]] - this would allow shifting disposal into a different thread
object Using {
	def pure[T](value:T):Using[T]	=
		new Using[T] {
			def use[U](handler:T=>U):U	= handler(value)
		}

	def delay[T](value: =>T):Using[T]	=
		new Using[T] {
			def use[U](handler:T=>U):U	= handler(value)
		}

	def resource[T](create: =>T)(implicit R:Resource[T]):Using[T]	=
		of(() => create)(R.dispose(_))

	def of[T](create: ()=>T)(dispose:T=>Unit):Using[T]	=
		new Using[T] {
			def use[X](handler:T=>X):X	= {
				var primary:Throwable	= null
				val value	= create()
				try {
					handler(value)
				}
				catch { case e:Throwable	=>
					primary	= e
					throw e
				}
				finally {
					if (primary ne null) {
						try {
							dispose(value)
						}
						catch { case e:Throwable	=>
							primary addSuppressed e
						}
					}
					else {
						dispose(value)
					}
				}
			}
		}

	implicit val UsingMonad:Monad[Using]	=
		new Monad[Using] {
			def pure[T](value:T):Using[T]								= Using pure value
			def flatMap[S,T](value:Using[S])(func:S=>Using[T]):Using[T]	= value flatMap func
		}

	implicit val UsingDelay:Delay[Using]	=
		new Delay[Using] {
			def delay[T](value: =>T):Using[T]	= Using delay value
		}
}

trait Using[T] { self =>
	def use[X](handler:T=>X):X

	final def run():T	= use(identity)

	final def ap[U,V](that:Using[U])(implicit ev:T=>U=>V):Using[V]	=
		zipWith(that) { (t,u) => t(u) }

	def pa[U](func:Using[T=>U]):Using[U] =
		func.zipWith(this) { (f,t) => f(t) }

	// aka tuple
	final def zip[U](that:Using[U]):Using[(T,U)]	=
		zipWith(that) { (t,u) => (t,u) }

	// aka map2
	final def zipWith[U,V](that:Using[U])(func:(T,U)=>V):Using[V]	=
		this flatMap { thisValue =>
			that map { thatValue =>
				func(thisValue, thatValue)
			}
		}

	final def map[U](func:T=>U):Using[U]	=
		new Using[U] {
			def use[X](handler:U=>X):X	= self use (func andThen handler)
		}

	final def flatMap[U](func:T=>Using[U]):Using[U]	=
		new Using[U] {
			def use[X](handler:U=>X):X	= self use (func(_) use handler)
		}
}
