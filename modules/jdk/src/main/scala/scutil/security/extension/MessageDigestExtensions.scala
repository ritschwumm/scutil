package scutil.io.extension

import java.security.*

import scutil.lang.ByteString

object MessageDigestExtensions {
	/** utility methods for MessageDigest objects */
	extension (peer:MessageDigest) {
		def updateByteString(it:ByteString):Unit	=
			peer.update(it.unsafeValue)

		def digestByteString():ByteString	=
			ByteString.unsafeFromArray(peer.digest())
	}
}
