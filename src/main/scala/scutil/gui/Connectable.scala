package scutil.gui

import scutil.Disposable

trait Connectable[S,T] {
	def connect(handler:S=>T):Disposable
	
	/*
	final def connectNullary(handler:Thunk[T]):Disposable =
			connect(ignorant(handler))
	*/
}
