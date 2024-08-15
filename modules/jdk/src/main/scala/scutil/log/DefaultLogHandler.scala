package scutil.log

import java.io.*

object DefaultLogHandler extends DefaultLogHandler

trait DefaultLogHandler extends LogHandler {
	def handle(event:LogEvent):Unit = {
		if (accept(event)) {
			print(formatter.format(event))
		}
	}

	def print(s:String):Unit = {
		synchronized {
			printStream.println(s)
			printStream.flush()
		}
	}

	def accept(event:LogEvent):Boolean	= true

	// TODO scala-js this make everything appear in console.error
	def printStream:PrintStream	= System.err

	def formatter:LogFormatter	= DefaultLogFormatter
}
