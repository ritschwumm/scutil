package scutil.platform

import java.io.File
import java.nio.charset.Charset

import scutil.lang._

object Platform {
	def lineSeparator:String	=
				SystemProperties.line.separator
			
	def fileEncoding:Tried[IllegalArgumentException,Charset]	=
				Charsets byName SystemProperties.file.encoding
	
	def unixRoot:File	= new File("/")
	def currentDir:File	= new File(SystemProperties.user.dir)
	def homeDir:File	= new File(SystemProperties.user.home)
	def tmpDir:File		= new File(SystemProperties.java.io.tmpdir)
}
