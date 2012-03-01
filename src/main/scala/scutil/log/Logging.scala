package scutil.log

import LogLevels._

trait Logging {
	final def FATAL(elements:Any*)	{ log(LogFatal,	elements) }
	final def ERROR(elements:Any*)	{ log(LogError,	elements) }
	final def WARN(elements:Any*)	{ log(LogWarn,	elements) }
	final def INFO(elements:Any*)	{ log(LogInfo,	elements) }
	final def DEBUG(elements:Any*)	{ log(LogDebug,	elements) }
	final def TRACE(elements:Any*)	{ log(LogTrace,	elements) }
	
	final def log(level:LogLevel, elements:Seq[Any]) { 
		// NOTE the hack
		//val	trace	= Thread.currentThread.getStackTrace.toList drop 5
		val here	= "scutil.log.Logging$class"
		val trace	= Thread.currentThread.getStackTrace.toList
						.dropWhile { _.getClassName != here }	// java.lang.Thread.getStackTrace
						.dropWhile { _.getClassName == here }	// LOG method
						.dropWhile { _.getClassName != here }	// LOG mixin forwarder
						.dropWhile { _.getClassName == here }	// ERROR method
						.drop(1)								// ERROR mixin forwarder
						.headOption
		val entry	= LogEntry(getClass, trace, level, elements) 
		logHandler log entry
	}
	
	def logHandler:LogHandler	= DefaultLogHandler
}
