package scutil.io.extension

import java.io.*

import scutil.lang.*

object DataOutputExtensions {
	implicit final class DataOutputExt(peer:DataOutput) {
		def writeByteString(it:ByteString):Unit	=
			peer.write(it.unsafeValue)

		def writeByteString(it:ByteString, offset:Int, length:Int):Unit	=
			peer.write(it.unsafeValue, offset, length)
	}
}
