package scutil.security

import java.security._

import scutil.lang._

object Hashing {
	// TODO throws NoSuchAlgorithmException
	def hash(algorithmName:String, rounds:Int, bytes:ByteString):ByteString	= {
		require(rounds >= 0, s"rounds must be non-negative, but was $rounds")

		val digest	= MessageDigest getInstance algorithmName
		var trip	= bytes.unsafeValue
		var round	= 0
		while (round < rounds) {
			digest update trip
			trip	= digest.digest()
			round	+= 1
		}
		ByteString unsafeFromArray trip
	}
}
