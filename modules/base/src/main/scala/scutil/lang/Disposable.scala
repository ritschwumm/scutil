package scutil.lang

import scutil.lang.tc._

object Disposable {
	def apply(todo:Thunk[Unit]):Disposable	= new TaskDisposable(todo)

	def delay(todo: =>Unit):Disposable		= new TaskDisposable(() => todo)

	/** forms a monoids with and */
	val empty:Disposable	=
		new Disposable {
			def dispose():Unit	= {}
		}

	def all(subs:Seq[Disposable]):Disposable	=
		delay {
			subs foreach {
				_.dispose()
			}
		}

	def allVar(subs:Disposable*):Disposable	= all(subs.toVector)

	def fromIo(io:Io[Unit]):Disposable	=
		Disposable(io.unsafeRun _)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val DisposableMonoid:Monoid[Disposable]			= Monoid.instance(Disposable.empty, _ and _)
	implicit def DisposableResource[T<:Disposable]:Resource[T]	= Resource instance (_.dispose())
}

/** something with a destructor */
trait Disposable {
	def dispose():Unit

	/** forms a monoid with empty */
	final def and(that:Disposable):Disposable	=
			 if (this == Disposable.empty)	that
		else if (that == Disposable.empty)	this
		else {
			Disposable delay {
				this.dispose()
				that.dispose()
			}
		}

	final def toIo:Io[Unit]	=
		Io delay { dispose() }
}

private final class TaskDisposable(task:Thunk[Unit]) extends Disposable {
	def dispose():Unit	= { task() }
}
