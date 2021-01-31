package scutil.lang

import  scala.util.Using.Releasable

import scutil.lang.tc._

object Using {
	def pure[T](value:T):Using[T]	=
		() => value -> Disposer.empty

	def delay[T](value: =>T):Using[T]	=
		() => value -> Disposer.empty

	@deprecated("use releasable", "0.200.0")
	def resource[T](create: =>T)(implicit R:Releasable[T]):Using[T]	=
		releasable(create)

	def releasable[T](create: =>T)(implicit R:Releasable[T]):Using[T]	=
		of(() => create)(R.release(_))

	def afterwards(dispose: =>Unit):Using[Unit]	=
		of(()=>())(_ => dispose)

	def of[T](create: ()=>T)(dispose:T=>Unit):Using[T]	=
		() => {
			val t	= create()
			val d	= Disposer delay { dispose(t) }
			t -> d
		}

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

	//------------------------------------------------------------------------------

	def unsafeBracket[T,U](resource:T)(dispose:T=>Unit)(consume:T=>U):U	= {
		var primary:Throwable	= null
		try {
			consume(resource)
		}
		catch { case e:Throwable	=>
			primary	= e
			throw e
		}
		finally {
			try {
				dispose(resource)
			}
			catch { case e:Throwable	=>
				if (primary ne null)	primary addSuppressed e
				else					throw e
			}
		}
	}
}

// TODO maybe this should just be Io[(T,Io[Unit])]
trait Using[T] {
	def open():(T, Disposer)

	final def openVoid():Disposer	= open()._2

	final def runVoid():Unit	= use(_ => ())

	final def use[X](handler:T=>X):X	=
		Using.unsafeBracket[(T,Disposer),X]
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

	final def foreach(func:T=>Unit):Unit	=
		use(func)
}
