package scutil.security

import java.security._

import scutil.lang._

object Hashing {
	@throws(classOf[NoSuchAlgorithmException])
	def hash(digestAlgorithm:String, roundCount:Int, bytes:ByteString):ByteString	= {
		require(roundCount > 0, s"rounds must be positive, but was ${roundCount.toString}")

		val digest	= MessageDigest getInstance digestAlgorithm
		var trip	= bytes.unsafeValue
		var round	= 0
		while (round < roundCount) {
			digest update trip
			trip	= digest.digest()
			round	+= 1
		}
		ByteString unsafeFromArray trip
	}
}
