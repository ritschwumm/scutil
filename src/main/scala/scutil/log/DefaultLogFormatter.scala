package scutil.log

import java.io._
import java.util.Date

import scutil.time._
import LogLevels._

object DefaultLogFormatter extends LogFormatter {
	def format(entry:LogEntry):String	= {
		val messages		= entry.elements collect { case x if !x.isInstanceOf[Throwable] => x }
		val messagesText	= 
				entry.level.name							+ "\t" + 
				"[" + Instant.now.toISO8601 + "]"			+ "\t" +
				// entry.source.getName						+ "\t" +
				// sourceMeaning(trace).getOrElse("?")		+ "\t" + 
				(sourceLocation(entry.trace) getOrElse "?")	+ "\t" +
				(messages mkString "\t")
				// (Text indent ("\t", messages mkString "\n")))
		
		
		val throwables		= entry.elements collect { case x:Throwable	=> x }
		val throwablesText	= {
			val	sw	= new StringWriter
			val	pw	= new PrintWriter(sw)
			throwables foreach {  _ printStackTrace pw }
			sw.toString
		}
		
		messagesText + (if (throwablesText.nonEmpty) "\n" else "") + throwablesText
	}
		
	private def sourceLocation(trace:Option[StackTraceElement]):Option[String] =
			for {
				top		<- trace
				file	<- Option(top.getFileName)
				line	= top.getLineNumber
				if (line >= 0)
			} 
			yield file + ":" + line
			
	/*
	private def sourceMeaning(trace:Option[StackTraceElement]):Option[String] =
			for {
				top	<- trace
			} 
			yield top.getClassName + "#" + top.getMethodName
	*/
}
