package scutil.lang

import scutil.lang.tc._

object Disposable extends DisposableInstances {
	def apply(todo:Task):Disposable		= new TaskDisposable(todo)
	
	def delay(todo: =>Unit):Disposable	= new TaskDisposable(() => todo)
	
	/** forms a monoids with and */
	val empty:Disposable	= EmptyDisposable
	
	def all(subs:ISeq[Disposable]):Disposable	=
			disposable {
				subs foreach {
					_.dispose()
				}
			}
			
	def allVar(subs:Disposable*):Disposable	= all(subs.toVector)
}

/** something with a destructor */
trait Disposable {
	def dispose():Unit
	
	/** forms a monoid with empty */
	final def and(that:Disposable):Disposable	=
			(this, that) match {
				case (EmptyDisposable, x)	=> x
				case (x, EmptyDisposable)	=> x
				case (a, b) =>
					disposable {
						a.dispose()
						b.dispose()
					}
			}
			
	final def toIo:Io[Unit]	=
			Io delay { dispose() }
}

private object EmptyDisposable extends Disposable {
	def dispose() {}
}

private final class TaskDisposable(task:Task) extends Disposable {
	def dispose() { task() }
}

trait DisposableInstances {
	implicit def DisposableResource[T<:Disposable]:Resource[T]	= Resource by (_.dispose())
}
