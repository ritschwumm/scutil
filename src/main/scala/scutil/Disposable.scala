package scutil

object Disposable {
	def apply(thunk: =>Unit):Disposable = new Disposable {
		def dispose() { thunk }
	}
	
	def fromThunk(thunk:()=>Unit):Disposable = new Disposable {
		def dispose() { thunk() }
	}
	
	val empty:Disposable	= new Disposable {
		def dispose() {}
	}
	
	def all(subs:Seq[Disposable]):Disposable	= new Disposable {
		def dispose() { 
			subs foreach {
				_.dispose() 
			}
		}
	}
}

trait Disposable {
	def dispose():Unit
}
