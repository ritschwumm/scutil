package scutil

import java.io._

object Files {
	def PWD:File	= new File(System getProperty "user.dir")
	def HOME:File	= new File(System getProperty "user.home")
	def TMP:File	= new File(System getProperty "java.io.tmpdir")
	def ROOT:File	= new File("/")
	
	def mkFileFilter(predicate:File=>Boolean) = new FileFilter {
		def accept(file:File):Boolean	= predicate(file)
	}
}
