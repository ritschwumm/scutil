package scutil.io.pimp

import java.io._

import scutil.lang._

object DataOutputImplicits extends DataOutputImplicits

trait DataOutputImplicits {
	implicit final class DataOutputExt(peer:DataOutput) {
		def writeByteString(it:ByteString):Unit	=
			peer write it.unsafeValue

		def writeByteString(it:ByteString, offset:Int, length:Int):Unit	=
			peer write (it.unsafeValue, offset, length)
	}
}
