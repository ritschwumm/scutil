package scutil.log

import LogLevels._

trait LogFormatter {
	def format(entry:LogEntry):String
}
