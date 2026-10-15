package scutil.gui

import scutil.lang.*

final class Caster[L] private[gui] (
	val addListener:Effect[L],
	val removeListener:Effect[L],
) {
	def listen(listener:L):Disposer = {
		{
			addListener(listener)
		}
		Disposer.delay {
			removeListener(listener)
		}
	}
}
