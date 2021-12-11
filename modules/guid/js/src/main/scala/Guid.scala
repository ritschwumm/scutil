package scutil.guid

import scala.scalajs.js.typedarray.*
import org.scalajs.dom.{ ByteString => _, _ }

import scutil.lang.*

object Guid extends GuidBase {
	protected def randomBytes(size:Int):ByteString	= {
		val buffer	= new Int8Array(size)
		webcrypto getRandomValues buffer
		ByteString unsafeFromArray buffer.toArray
	}
}
