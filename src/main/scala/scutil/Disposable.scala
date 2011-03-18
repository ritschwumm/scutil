package scutil

object Disposable {
	def apply(thunk: =>Unit) = new Disposable {
		def dispose() { thunk }
	}
	
	def fromThunk(thunk:()=>Unit) = new Disposable {
		def dispose() { thunk() }
	}
}

trait Disposable {
	def dispose():Unit
}
