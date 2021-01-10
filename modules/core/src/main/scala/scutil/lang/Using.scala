package scutil.lang

import scutil.lang.tc._

object Using {
	def pure[T](value:T):Using[T]	=
		() => value -> Disposable.empty

	def delay[T](value: =>T):Using[T]	=
		() => value -> Disposable.empty

	def of[T](create: ()=>T)(dispose:T=>Unit):Using[T]	=
		() => {
			val t	= create()
			val d	= Disposable delay { dispose(t) }
			t -> d
		}

	def resource[T](create: =>T)(implicit R:Resource[T]):Using[T]	=
		of(() => create)(R.dispose(_))

	def afterwards(dispose: =>Unit):Using[Unit]	=
		of(()=>())(_ => dispose)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val UsingMonad:Monad[Using]	=
		new Monad[Using] {
			def pure[T](value:T):Using[T]									= Using pure value
			def flatMap[S,T](value:Using[S])(func:S=>Using[T]):Using[T]	= value flatMap func
		}

	implicit val UsingDelay:Delay[Using]	=
		new Delay[Using] {
			def delay[T](value: =>T):Using[T]	= Using delay value
		}
}

// TODO maybe this should just be Io[(T,Io[Unit])]
trait Using[T] {
	def open():(T, Disposable)

	final def run():T	= use(identity)

	final def use[X](handler:T=>X):X	=
		Resource.bracket[(T,Disposable),X]
			{ open() }
			{ case (t,d) => d.dispose()	}
			{ case (t,d) => handler(t)	}

	final def map[U](func:T=>U):Using[U]	=
		flatMap(func andThen Using.pure)

	final def ap[U,V](that:Using[U])(implicit ev:T=>U=>V):Using[V]	=
		map2(that) { (t,u) => t(u) }

	final def product[U](that:Using[U]):Using[(T,U)]	=
		map2(that) { (t,u) => (t,u) }

	final def map2[U,V](that:Using[U])(func:(T,U)=>V):Using[V]	=
		for { thisValue <- this; thatValue <- that }
		yield func(thisValue, thatValue)

	final def flatMap[U](func:T=>Using[U]):Using[U]	=
		() => {
			val (t, dt)	= open()
			try {
				val (u, du)	= func(t).open()
				// disposes the inner first, and prefers
				// throwing the inner exception, too (!)
				val d	= du combine dt
				u -> d
			}
			catch { case eu:Throwable =>
				// if opening the inner resource failed we still have
				// to close the outer resource, which might fail, too
				try {
					dt.dispose()
				}
				catch { case et:Throwable =>
					eu.addSuppressed(et)
				}
				throw eu
			}
		}

	final def foreach(func:T=>Unit):Using[Unit]	=
		map(func)
}
