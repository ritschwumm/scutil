package scutil.log

import java.io._

import LogLevelEnum._
	
object DefaultLogHandler extends LogHandler {
	def log(entry:LogEntry) {
		synchronized {
			val string	= DefaultLogFormatter format entry
			System.err println string
		}
	}
}
