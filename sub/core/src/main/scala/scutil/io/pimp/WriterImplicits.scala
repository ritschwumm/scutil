package scutil.io.pimp

import java.io._

object WriterImplicits extends WriterImplicits

trait WriterImplicits {
	/** utility methods for Writer objects */
	implicit final class WriterExt(peer:Writer) {
		def buffered:BufferedWriter	=
				new BufferedWriter(peer)
	}
}
