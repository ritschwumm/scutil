package scutil.log

import java.io.PrintStream

final class DefaultLogger(stream:PrintStream) extends Logger {
	import LogLevels._
	
	def log(trace:Option[StackTraceElement], level:LogLevel, elements:Seq[Any]) {
		val messages	= elements collect { case x if !x.isInstanceOf[Throwable] => x }
		stream println (
				level.name + "\t" + 
				// sourceMeaning(trace).getOrElse("?") + "\t" + 
				(sourceLocation(trace) getOrElse "?") + "\t" +
				(messages mkString "\t"))
				// (Text indent ("\t", messages mkString "\n")))
		
		val throwables	= elements collect { case x:Throwable	=> x}
		throwables foreach  { _.printStackTrace(stream) } 
		// { stream.println(_.getStackTraceString.index) }
	}
	
	private def sourceLocation(trace:Option[StackTraceElement]):Option[String] =
			for {
				top		<- trace
				file	<- Option(top.getFileName)
				line	<- if (top.getLineNumber >= 0) Some(top.getLineNumber) else None
			} 
			yield file + ":" + line
			
	private def sourceMeaning(trace:Option[StackTraceElement]):Option[String] =
			for {
				top		<- trace
			} 
			yield top.getClassName + "#" + top.getMethodName
	
	/*
	private def throwableString(e:Throwable):String = {
		val	sw	= new StringWriter
		e printStackTrace new PrintWriter(sw)
		sw.toString
	}
	*/
}
