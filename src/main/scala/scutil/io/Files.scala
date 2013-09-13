package scutil.io

import java.io._

import scutil.platform.SystemProperties

object Files {
	def UNIX_ROOT:File	= new File("/")
	
	def PWD:File		= new File(SystemProperties.user.dir)
	def HOME:File		= new File(SystemProperties.user.home)
	def TMP:File		= new File(SystemProperties.java.io.tmpdir)
}
