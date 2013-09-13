package scutil.gui

import java.awt.{ List=>AwtList, _ }
import java.awt.event._

import scutil.lang._

object GlobalAWTEvent {
	def connect(handler:AWTEvent=>Unit):Disposable = {
		val listener	= new AWTEventListener {
			def eventDispatched(ev:AWTEvent)	= handler(ev)
		}
		{
			install(listener)
		}
		disposable {
			uninstall(listener)
		}
	}
	
	private def install(listener:AWTEventListener) {
		Toolkit.getDefaultToolkit addAWTEventListener (listener, -1) 
	}
	
	private def uninstall(listener:AWTEventListener) {
		Toolkit.getDefaultToolkit removeAWTEventListener listener
	}
}
