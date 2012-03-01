package scutil.log

import LogLevels._

trait LogHandler {
	def log(entry:LogEntry):Unit
}
