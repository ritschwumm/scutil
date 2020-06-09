package scutil.gui

import java.awt.{ AWTEvent, Toolkit }
import java.awt.event._

import scutil.lang._

object GlobalAWTEvent {
	def connect(mask:Long)(handler:AWTEvent=>Unit):Disposable = {
		val listener	=
			new AWTEventListener {
				def eventDispatched(ev:AWTEvent):Unit	= handler(ev)
			}
		val toolkit	= Toolkit.getDefaultToolkit
		toolkit addAWTEventListener (listener, mask)
		disposable {
			toolkit removeAWTEventListener listener
		}
	}
}
