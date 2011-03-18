package scutil.gui

import java.awt.{ List=>AwtList, _ }
import java.awt.event._

import scutil.Disposable

object GlobalAWTEvent extends Connectable[AWTEvent,Unit] {
	def connect(handler:AWTEvent=>Unit):Disposable = {
		val listener	= new AWTEventListener {
			def eventDispatched(ev:AWTEvent)	= handler(ev)
		}
		install(listener)
		new Disposable {
			def dispose() {
				uninstall(listener)
			}
		}
	}
	
	private def install(listener:AWTEventListener) {
		Toolkit.getDefaultToolkit addAWTEventListener (listener, -1) 
	}
	
	private def uninstall(listener:AWTEventListener) {
		Toolkit.getDefaultToolkit removeAWTEventListener listener
	}
}
