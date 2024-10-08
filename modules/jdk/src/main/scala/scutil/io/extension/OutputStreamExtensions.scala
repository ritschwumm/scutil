package scutil.io.extension

import java.io.*
import java.nio.charset.Charset

import scutil.lang.ByteString

object OutputStreamExtensions {
	/** utility methods for OutputStream objects */
	extension (peer:OutputStream) {
		def writeByteString(it:ByteString):Unit	=
			peer.write(it.unsafeValue)

		def writeByteString(it:ByteString, offset:Int, length:Int):Unit	=
			peer.write(it.unsafeValue, offset, length)

		def buffered:BufferedOutputStream	=
			new BufferedOutputStream(peer)

		def toWriter(encoding:Charset):OutputStreamWriter	=
			new OutputStreamWriter(peer, encoding)
	}
}
