package scutil.pimp

import java.io._

object ThrowableImplicits extends ThrowableImplicits

trait ThrowableImplicits {
    implicit def toThrowableExt(delegate:Throwable)	= new ThrowableExt(delegate)
}

final class ThrowableExt(delegate:Throwable) {
	def stackTrace:String	= {
		val	sw	= new StringWriter
		val pw	= new PrintWriter(sw)
		delegate printStackTrace pw
		sw.toString
	}
}
