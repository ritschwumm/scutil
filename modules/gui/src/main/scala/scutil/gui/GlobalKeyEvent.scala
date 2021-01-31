package scutil.gui

import java.awt.{ KeyboardFocusManager, KeyEventDispatcher }
import java.awt.event._

import scutil.lang._

object GlobalKeyEvent {
	// TODO using this is a Using
	def connect(handler:KeyEvent=>Boolean):Disposer = {
		val dispatcher	=
			new KeyEventDispatcher {
				def dispatchKeyEvent(ev:KeyEvent):Boolean	= handler(ev)
			}
		val focusManager	= KeyboardFocusManager.getCurrentKeyboardFocusManager
		// addKeyEventPostProcessor
		focusManager addKeyEventDispatcher dispatcher
		Disposer delay {
			focusManager removeKeyEventDispatcher dispatcher
		}
	}
}
