package scutil.guid

import java.util.concurrent.atomic.AtomicInteger

import scutil.lang.*
import scutil.codec.*

protected abstract class GuidBase {
	def fresh[F[_]:Delay]:F[String]	=
		Delay[F].delay(unsafeFresh())

	def unsafeFresh():String	= {
		val rand	= Hex.encodeByteString(randomBytes(randomCount))
		val stamp	= Hex.encodeByteString(timestamp())
		val count	= Hex.encodeByteString(counterValue())
		rand + "-" + stamp + "-" + count
	}

	//------------------------------------------------------------------------------

	private val randomCount	= 8

	private val counter	= new AtomicInteger(0)

	protected def randomBytes(size:Int):ByteString

	private def timestamp():ByteString	=
		ByteString.fromBigEndianLong(System.currentTimeMillis())

	private def counterValue():ByteString	=
		ByteString.fromBigEndianInt(counter.incrementAndGet())
}
