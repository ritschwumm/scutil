package scutil.platform

import java.nio.file.Path
import java.nio.charset.Charset

import scutil.lang.*

object Platform {
	def lineSeparator:String	=
		SystemProperties.line.separator

	def fileEncoding:Either[IllegalArgumentException,Charset]	=
		Charsets.byName(SystemProperties.file.encoding)

	//------------------------------------------------------------------------------

	def unixRoot:Path	= Path.of("/")
	def currentDir:Path	= Path.of(SystemProperties.user.dir)
	def homeDir:Path	= Path.of(SystemProperties.user.home)
	def tmpDir:Path		= Path.of(SystemProperties.java.io.tmpdir)
	def javaHome:Path	= Path.of(SystemProperties.java.home)

	//------------------------------------------------------------------------------

	def property(name:String):Option[String]	=
		Option(System.getProperty(name))

	def env(name:String):Option[String]	=
		Option(System.getenv(name))
}
