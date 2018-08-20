package scutil.guid

import java.security.SecureRandom

import scutil.base.implicits._
import scutil.lang._

object Guid extends GuidBase {
	// NOTE SecureRandom getInstance "SHA1PRNG" works. too
	// NOTE SecureRandom.getInstanceStrong() make this hang
	// NOTE SecureRandom is guaranteed to be thread safe
	private val random	= new SecureRandom()
	
	protected def randomBytes(size:Int):ByteString	=
			random byteString size
}
