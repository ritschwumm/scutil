package scutil.io.extension

import java.io.*

import scutil.lang.*

object DataOutputExtensions {
	extension (peer:DataOutput) {
		def writeByteString(it:ByteString):Unit	=
			peer.write(it.unsafeValue)

		def writeByteString(it:ByteString, offset:Int, length:Int):Unit	=
			peer.write(it.unsafeValue, offset, length)
	}
}
