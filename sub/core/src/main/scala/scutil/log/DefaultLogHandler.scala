package scutil.log

import java.io._

import scutil.lang.SourceLocation
import scutil.time._

object DefaultLogHandler extends DefaultLogHandler

trait DefaultLogHandler extends LogHandler {
	def handle(event:LogEvent) {
		if (accept(event)) {
			print(format(event))
		}
	}
	
	def accept(event:LogEvent):Boolean	= true
	
	def print(s:String) {
		synchronized {
			printStream println	s
			printStream flush	()
		}
	}
	
	def printStream:PrintStream	=
			System.err
	
	def format(event:LogEvent):String	= {
		val messages	= event.elements flatMap extractMessage
		val throwables	= event.elements flatMap extractThrowable
		
		val headerItems	=
				Vector(
					formatLevel(event.level),
					formatInstant(event.timestamp),
					formatLocation(event.location)
				)
		val messageItems	= messages map formatMessage
		val throwableItems	= throwables map formatThrowable
			
		((headerItems ++ messageItems) mkString "\t")	+
		(if (throwableItems.nonEmpty) "\n" else "")		+
		(throwableItems mkString "")
	}
	
	def formatInstant(it:MilliInstant):String	=
			"[" + it.toISO8601 + "]"
		
	def formatLocation(it:SourceLocation):String	=
			it.name + ":" + it.line
		
	def formatLevel(it:LogLevel):String	=
			it match {
				case TRACE	=> "TRACE"
				case DEBUG	=> "DEBUG"
				case INFO	=> "INFO"
				case WARN	=> "WARN"
				case ERROR	=> "ERROR"
				case FATAL	=> "FATAL"
			}
		
	def formatMessage(it:Any):String	=
			if (it != null)	it.toString
			else			"<null>"
			
	def formatThrowable(t:Throwable):String	= {
		val	sw	= new StringWriter
		val	pw	= new PrintWriter(sw)
		t printStackTrace pw
		sw.toString
	}
	
	def extractMessage(element:Any):Option[Any]	=
			element match {
				case x if !x.isInstanceOf[Throwable]	=> Some(x)
				case _									=> None
			}
		
	def extractThrowable(element:Any):Option[Throwable]	=
			element match {
				case x:Throwable 	=> Some(x)
				case _				=> None
			}
}
