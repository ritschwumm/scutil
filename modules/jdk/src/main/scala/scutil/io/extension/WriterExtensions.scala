package scutil.io.extension

import java.io.*

object WriterExtensions {
	/** utility methods for Writer objects */
	implicit final class WriterExt(peer:Writer) {
		def buffered:BufferedWriter	= new BufferedWriter(peer)
	}
}
