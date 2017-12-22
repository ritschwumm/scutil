package scutil.log

import java.io._

import scutil.base.implicits._
import scutil.jtime.implicits._
import scutil.lang._
import scutil.time._

object DefaultLogHandler extends DefaultLogHandler

trait DefaultLogHandler extends LogHandler {
	def handle(event:LogEvent) {
		if (accept(event)) {
			print(formatEvent(event))
		}
	}
	
	def accept(event:LogEvent):Boolean	= true
	
	def print(s:String) {
		synchronized {
			printStream println	s
			printStream flush	()
		}
	}
	
	// TODO scala-js this make everything appear in console.error
	def printStream:PrintStream	=
			System.err
	
	//------------------------------------------------------------------------------
		
	// TODO scala-js in the browser it might make more sense to call console.log with individual elements
	def formatEvent(event:LogEvent):String	= {
		val atoms		= event.values flatMap (_.atoms)
		val messages	= atoms collect { case LogString(x) 	=> x }
		val throwables	= atoms collect { case LogThrowable(x)	=> x }
		
		val headerItems	=
				Vector(
					formatLevel(event.level),
					formatInstant(event.timestamp),
					formatLocation(event.location)
				)
		val messageItems	= messages		map formatMessage
		val throwableItems	= throwables	map formatThrowable
			
		((headerItems ++ messageItems) mkString "\t")	+
		(if (throwableItems.nonEmpty) "\n" else "")		+
		(throwableItems mkString "")
	}
	
	def formatInstant(it:MilliInstant):String	=
			"[" + it.toISO8601 + "]"
		
	def formatLocation(it:SourceLocation):String	=
			it.name + ":" + it.line.toString
		
	def formatLevel(it:LogLevel):String	=
			it match {
				case TRACE	=> "TRACE"
				case DEBUG	=> "DEBUG"
				case INFO	=> "INFO"
				case WARN	=> "WARN"
				case ERROR	=> "ERROR"
				case FATAL	=> "FATAL"
			}
		
	def formatMessage(it:String):String	=
			if (it != null)	it.toString
			else			"<null>"
			
	// TODO scala-js this is not very useful in the browser
	def formatThrowable(it:Throwable):String	=
			if (it != null)	it.stackTrace
			else			"<null>"
}
