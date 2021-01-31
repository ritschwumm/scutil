package scutil.gui

import java.awt.{ AWTEvent, Toolkit }
import java.awt.event._

import scutil.lang._

object GlobalAWTEvent {
	// TODO using this is a Using
	def connect(mask:Long)(handler:AWTEvent=>Unit):Disposer = {
		val listener	=
			new AWTEventListener {
				def eventDispatched(ev:AWTEvent):Unit	= handler(ev)
			}
		val toolkit	= Toolkit.getDefaultToolkit
		toolkit.addAWTEventListener(listener, mask)
		Disposer delay {
			toolkit removeAWTEventListener listener
		}
	}
}
