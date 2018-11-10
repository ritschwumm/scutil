package scutil.guid

import java.util.concurrent.atomic.AtomicInteger

import scutil.lang._

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
			(ByteString makeWithByteBuffer 8) { buffer =>
				buffer putLong System.currentTimeMillis()
			}

	private def counterValue():ByteString	=
			(ByteString makeWithByteBuffer 4) { buffer =>
				buffer putInt counter.incrementAndGet()
			}
}
