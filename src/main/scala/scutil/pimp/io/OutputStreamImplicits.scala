package scutil.pimp

import java.io._
import java.nio.charset.Charset

object OutputStreamImplicits extends OutputStreamImplicits

trait OutputStreamImplicits {
    implicit def toOutputStreamExt(peer:OutputStream)	= new OutputStreamExt(peer)
}

/** utility methods for OutputStream objects */ 
final class OutputStreamExt(peer:OutputStream) {
	def toWriter(encoding:Charset):OutputStreamWriter	=
			new OutputStreamWriter(peer, encoding)
}
