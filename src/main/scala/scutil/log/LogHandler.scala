package scutil.log

trait LogHandler {
	def handle(event:LogEvent):Unit
}
