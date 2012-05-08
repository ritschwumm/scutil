package scutil.gui

import scutil.Functions._
import scutil.Connectable
import scutil.Disposable

abstract class Caster[L,E] private[gui] (
	addListener:Effect[L], 
	removeListener:Effect[L],
	createListener:Effect[E]=>L
) extends Connectable[E,Unit] {
	def connect(callback:E=>Unit):Disposable = 
			listen(createListener(callback))
	
	def listen(listener:L):Disposable = {
		{
			addListener(listener)
		}
		Disposable {
			removeListener(listener)
		}
	}
}
