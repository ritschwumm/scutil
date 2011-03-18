package scutil.log

object Log {
	private val defaultLogger	= new DefaultLogger(System.err)
	def logger(source:Class[_]):Logger	= defaultLogger
}
