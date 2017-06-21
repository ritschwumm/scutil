package scutil.io.pimp

import java.io._
import java.nio.charset.Charset

object OutputStreamImplicits extends OutputStreamImplicits

trait OutputStreamImplicits {
	/** utility methods for OutputStream objects */
	implicit final class OutputStreamExt(peer:OutputStream) {
		def buffered:BufferedOutputStream	=
				new BufferedOutputStream(peer)
			
		def toWriter(encoding:Charset):OutputStreamWriter	=
				new OutputStreamWriter(peer, encoding)
	}
}
