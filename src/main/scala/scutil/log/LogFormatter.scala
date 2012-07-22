package scutil.log

trait LogFormatter {
	def format(entry:LogEntry):String
}
