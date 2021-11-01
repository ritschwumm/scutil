package scutil.io.extension

import java.security._

import scutil.lang.ByteString

object MessageDigestImplicits extends MessageDigestImplicits

trait MessageDigestImplicits {
	/** utility methods for MessageDigest objects */
	implicit final class MessageDigestExt(peer:MessageDigest) {
		def updateByteString(it:ByteString):Unit	=
			peer.update(it.unsafeValue)

		def digestByteString():ByteString	=
			ByteString unsafeFromArray peer.digest()
	}
}
