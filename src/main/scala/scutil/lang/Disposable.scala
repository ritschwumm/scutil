package scutil.lang

object Disposable {
	def apply(todo: =>Unit):Disposable	= new FunctionDisposable(task(todo))
	def fromThunk(todo:Task):Disposable	= new FunctionDisposable(todo)
	
	/** forms a monoid with append */
	val empty:Disposable	= EmptyDisposable
	
	def all(subs:Seq[Disposable]):Disposable	=
			Disposable {
				subs foreach {
					_.dispose() 
				}
			}
}

/** something with a destructor */
trait Disposable {
	def dispose():Unit
	
	/** forms a monoid with empty */
	final def append(that:Disposable):Disposable	=
			Disposable {
				this.dispose()
				that.dispose()
			}
}

private object EmptyDisposable extends Disposable {
	def dispose() {}
}

private final class FunctionDisposable(task:Task) extends Disposable {
	def dispose() { task() }
}
