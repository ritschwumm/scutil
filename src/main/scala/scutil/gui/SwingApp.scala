package scutil.gui

import scutil.log._
import scutil.ThreadUtil._
import scutil.gui.SwingUtil._

/** an App starting in the event dispatch thread */
trait SwingApp extends App with Logging  {
	final override def main(args:Array[String]) {
		edt {
			try {
				// installDefaultUncaughtExceptionHandler { 
				// 	(t,e)	=> ERROR(e)  
				// }
				super.main(args)
			}
			catch {
				case e	=> ERROR(e)
			}
		}
	}
}
