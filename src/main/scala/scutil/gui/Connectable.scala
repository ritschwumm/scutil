package scutil.gui

import scutil.Functions._
import scutil.Disposable

trait Connectable[S,T] {
	def connect(handler:S=>T):Disposable
	
	def connectNullary(callback: ()=>T):Disposable =
			connect(ignorant(callback))
}
