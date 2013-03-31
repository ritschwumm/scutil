package scutil.gui

import scutil.gui.SwingUtil._
import scutil.log._

/** an App starting in the event dispatch thread */
trait SwingApp extends App {
	final override def main(args:Array[String]) {
		edt {
			try {
				super.main(args)
			}
			catch { case e:Exception =>
				onStartupError(e)
			}
		}
	}
	
	def onStartupError(e:Exception):Unit
}
