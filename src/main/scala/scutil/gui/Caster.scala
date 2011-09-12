package scutil.gui

import scutil.Disposable
import scutil.Types._

abstract class Caster[L,E] private[gui] (addListener:Effect[L], removeListener:Effect[L], createListener:Effect[E]=>L) extends Connectable[E,Unit] {
	def connect(callback:E=>Unit):Disposable = 
			connect(createListener(callback))
	
	def connect(listener:L):Disposable = {
		addListener(listener)
		new Disposable {
			def dispose() {
				removeListener(listener)
			}
		}
	}
}
