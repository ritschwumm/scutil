package scutil.gui

import scutil.lang._

abstract class Caster[L,E] private[gui] (
	addListener:Effect[L],
	removeListener:Effect[L],
	createListener:Effect[E]=>L
) {
	def connect(callback:E=>Unit):Disposer =
		listen(createListener(callback))

	def listen(listener:L):Disposer = {
		{
			addListener(listener)
		}
		Disposer delay {
			removeListener(listener)
		}
	}
}
