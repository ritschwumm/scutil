package scutil.lang.extension

import java.io.*

object ThrowableExtensions {
	extension (peer:Throwable) {
		def causeOption:Option[Throwable]	=
			Option(peer.getCause)

		def stackTrace:String	= {
			val	sw	= new StringWriter
			peer.printStackTrace(new PrintWriter(sw))
			sw.toString
		}
	}
}
