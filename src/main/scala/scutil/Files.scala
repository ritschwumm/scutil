package scutil

import java.io._

object Files {
	def UNIX_ROOT:File	= new File("/")
	
	def PWD:File		= new File(SystemProperties.user.dir)
	def HOME:File		= new File(SystemProperties.user.home)
	def TMP:File		= new File(SystemProperties.java.io.tmpdir)
}
