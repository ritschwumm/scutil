package scutil

import java.io._

object Files {
	def UNIX_ROOT:File	= new File("/")
	
	def PWD:File		= new File(Platform.user.dir)
	def HOME:File		= new File(Platform.user.home)
	def TMP:File		= new File(Platform.java.io.tmpdir)
}
