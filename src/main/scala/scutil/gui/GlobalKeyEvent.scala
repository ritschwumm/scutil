package scutil.gui

import java.awt.{ List=>AwtList, _ }
import java.awt.event._

import scutil.Disposable

object GlobalKeyEvent extends Connectable[KeyEvent,Boolean] {
	def connect(handler:KeyEvent=>Boolean):Disposable = {
		val dispatcher	= new KeyEventDispatcher {
			def dispatchKeyEvent(ev:KeyEvent):Boolean	= handler(ev)
		}
		install(dispatcher)
		new Disposable {
			def dispose() {
				uninstall(dispatcher)
			}
		}
	}
	
	private def install(dispatcher:KeyEventDispatcher) {
		KeyboardFocusManager.getCurrentKeyboardFocusManager addKeyEventDispatcher dispatcher
	}
	
	private def uninstall(dispatcher:KeyEventDispatcher) {
		KeyboardFocusManager.getCurrentKeyboardFocusManager removeKeyEventDispatcher dispatcher
	}
	
	// KeyboardFocusManager.getCurrentKeyboardFocusManager addKeyEventPostProcessor
}
