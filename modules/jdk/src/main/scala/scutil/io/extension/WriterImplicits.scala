package scutil.io.extension

import java.io._

object WriterImplicits {
	/** utility methods for Writer objects */
	implicit final class WriterExt(peer:Writer) {
		def buffered:BufferedWriter	= new BufferedWriter(peer)
	}
}
