package scutil.guid

import scala.scalajs.js.typedarray._
import org.scalajs.dom.{ ByteString => _, _ }

import scutil.lang._

object Guid extends GuidBase {
	protected def randomBytes(size:Int):ByteString	= {
		val buffer	= new Int8Array(size)
		webcrypto getRandomValues buffer
		ByteString unsafeFromArray buffer.toArray
	}
}
