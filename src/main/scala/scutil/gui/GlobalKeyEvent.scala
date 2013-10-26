package scutil.gui

import java.awt.{ List=>AwtList, _ }
import java.awt.event._

import scutil.lang._

object GlobalKeyEvent {
	def connect(handler:KeyEvent=>Boolean):Disposable = {
		val dispatcher	= 
				new KeyEventDispatcher {
					def dispatchKeyEvent(ev:KeyEvent):Boolean	= handler(ev)
				}
		val focusManager	= KeyboardFocusManager.getCurrentKeyboardFocusManager
		// addKeyEventPostProcessor
		focusManager addKeyEventDispatcher dispatcher
		disposable {
			focusManager removeKeyEventDispatcher dispatcher
		}
	}
}
