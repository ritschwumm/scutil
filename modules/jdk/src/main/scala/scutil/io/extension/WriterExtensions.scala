package scutil.io.extension

import java.io.*

object WriterExtensions {
	/** utility methods for Writer objects */
	extension (peer:Writer) {
		def buffered:BufferedWriter	= new BufferedWriter(peer)
	}
}
