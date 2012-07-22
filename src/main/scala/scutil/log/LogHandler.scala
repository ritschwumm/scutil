package scutil.log

trait LogHandler {
	def log(entry:LogEntry):Unit
}
