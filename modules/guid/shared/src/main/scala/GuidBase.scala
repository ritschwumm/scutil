package scutil.guid

import java.util.concurrent.atomic.AtomicInteger

import scutil.lang._
import scutil.codec._

protected abstract class GuidBase {
	private val randomCount	= 8

	private val counter	= new AtomicInteger(0)

	val freshIo:Io[String]	= Io delay fresh()

	def fresh():String	= {
		val rand	= Hex encodeByteString randomBytes(randomCount)
		val stamp	= Hex encodeByteString timestamp()
		val count	= Hex encodeByteString counterValue()
		rand + "-" + stamp + "-" + count
	}

	protected def randomBytes(size:Int):ByteString

	private def timestamp():ByteString	=
		ByteString fromBigEndianLong System.currentTimeMillis()

	private def counterValue():ByteString	=
		ByteString fromBigEndianInt counter.incrementAndGet()
}
