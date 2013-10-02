package scutil.log

import java.io._

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
			System.err println	s
			System.err flush	()
		}
	}
	
	def format(event:LogEvent):String	= {
		val first	= header(event) ++ messages(event.elements) mkString "\t"
		val more	= throwables(event.elements) map trace  mkString ""
		first +	
		(if (more.nonEmpty) "\n" else "")	+
		more
	}
	
	def messages(elements:Seq[Any]):Seq[String]	=
			elements collect { case x if !x.isInstanceOf[Throwable] => x.toString }
		
	def throwables(elements:Seq[Any]):Seq[Throwable]	=
			elements collect { case x:Throwable => x }
	
	def header(event:LogEvent):Seq[String]	=
			Vector(event.level.name, s"[${now}]", event.location.toString)
	
	def now:String	=
			MilliInstant.now.toISO8601
		
	def trace(t:Throwable):String	= {
		val	sw	= new StringWriter
		val	pw	= new PrintWriter(sw)
		t printStackTrace pw
		sw.toString
	}
}
