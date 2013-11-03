package scutil.pimp

import java.io._

object ThrowableImplicits extends ThrowableImplicits

trait ThrowableImplicits {
    implicit def toThrowableExt(peer:Throwable)	= new ThrowableExt(peer)
}

final class ThrowableExt(peer:Throwable) {
	def causeOption:Option[Throwable]	=
			Option(peer.getCause)
		
	def stackTrace:String	= {
		val	sw	= new StringWriter
		val pw	= new PrintWriter(sw)
		peer printStackTrace pw
		sw.toString
	}
}
