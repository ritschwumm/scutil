package scutil.lang

object Disposable {
	def apply(todo:Task):Disposable	= new TaskDisposable(todo)
	
	/** forms monoids with disposeBefore and disposeAfter */
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
	final def disposeBefore(that:Disposable):Disposable	=
			disposable {
				this.dispose()
				that.dispose()
			}
			
	/** forms a monoid with empty */
	final def disposeAfter(that:Disposable):Disposable	=
			disposable {
				that.dispose()
				this.dispose()
			}
}

private object EmptyDisposable extends Disposable {
	def dispose() {}
}

private final class TaskDisposable(task:Task) extends Disposable {
	def dispose() { task() }
}
