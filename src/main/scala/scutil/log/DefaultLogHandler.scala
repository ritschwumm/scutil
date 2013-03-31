package scutil.log

import java.io._

import scutil.time._

object DefaultLogHandler extends DefaultLogHandler

trait DefaultLogHandler extends LogHandler {
	def handle(event:LogEvent) {
		write(format(event))
	}
	
	def format(event:LogEvent):String	= {
		val messages		= event.elements collect { case x if !x.isInstanceOf[Throwable] => x }
		val messagesText	= 
				event.level.name					+:
				("[" + Instant.now.toISO8601 + "]")	+:
				event.location.toString				+:
				messages							mkString
				"\t"
		
		val throwables		= event.elements collect { case x:Throwable	=> x }
		val throwablesText	= {
			val	sw	= new StringWriter
			val	pw	= new PrintWriter(sw)
			throwables foreach {  _ printStackTrace pw }
			sw.toString
		}
		
		messagesText															+
		(if (messagesText.nonEmpty && throwablesText.nonEmpty) "\n" else "")	+ 
		throwablesText
	}
		
	def write(s:String) {
		synchronized {
			System.err println s
		}
	}
}
