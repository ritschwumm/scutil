package scutil

object Connectable {
	def fromFunction[S,T](connectFunc:(S=>T)=>Disposable)	= new Connectable[S,T] {
		def connect(handler:S=>T):Disposable	= connectFunc(handler)
	}
	
	/*
	final def connectNullary(handler:Thunk[T]):Disposable =
			connect(ignorant(handler))
	*/
}

trait Connectable[+S,-T] { outer =>
	def connect(handler:S=>T):Disposable
	
	final def orElse[S1>:S,T1<:T](that:Connectable[S1,T1]):Connectable[S1,T1]	= new Connectable[S1,T1] {
		def connect(handler:S1=>T1):Disposable = {
			val	thisD	= outer	connect handler
			val	thatD	= that	connect handler
			Disposable {
				thisD.dispose()
				thatD.dispose()
			} 
		}
	}
}
