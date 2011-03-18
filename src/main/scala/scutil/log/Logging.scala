package scutil.log

trait Logging { self =>
	import LogLevels._
	
	final def DEBUG(elements:Any*)	{ LOG(LogLevels.DEBUG,	elements) }
	final def INFO(elements:Any*)	{ LOG(LogLevels.INFO,	elements) }
	final def WARN(elements:Any*)	{ LOG(LogLevels.WARN,	elements) }
	final def ERROR(elements:Any*)	{ LOG(LogLevels.ERROR,	elements) }
	
	final def LOG(level:LogLevel, elements:Seq[Any]) { 
		//val	trace	= Thread.currentThread.getStackTrace.toList drop 5
		val here	= "scutil.log.Logging$class"
		val trace	= Thread.currentThread.getStackTrace.toList
						.dropWhile { _.getClassName != here }	// java.lang.Thread.getStackTrace
						.dropWhile { _.getClassName == here }	// LOG method
						.dropWhile { _.getClassName != here }	// LOG mixin forwarder
						.dropWhile { _.getClassName == here }	// ERROR method
						.drop(1)								// ERROR mixin forwarder
						.headOption
		logger log (trace, level, elements) 
	}
	
	protected val logger:Logger	= Log logger getClass
}
