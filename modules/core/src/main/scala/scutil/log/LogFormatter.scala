package scutil.log

trait LogFormatter {
	def format(event:LogEvent):String
}
