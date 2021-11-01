package scutil.guid

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.typedarray._
import org.scalajs.dom.{ ByteString => _, _ }

import scutil.lang._

object Guid extends GuidBase {
	// org.scalajs.dom.crypto.GlobalCrypto.crypto
	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	private lazy val undefOrCrypto:UndefOr[Crypto]	=
		// TODO crypto does not work - why?
		//webcrypto.asInstanceOf[UndefOr[Crypto]]	orElse
		js.Dynamic.global.crypto.asInstanceOf[UndefOr[Crypto]]	orElse
		js.Dynamic.global.msCrypto.asInstanceOf[UndefOr[Crypto]]

	protected def randomBytes(size:Int):ByteString	=
		undefOrCrypto.fold(
			insecureRandomBytes(size)
		)(
			secureRandomBytes(size)
		)

	private def secureRandomBytes(size:Int)(crypt:Crypto):ByteString	= {
		val buffer	= new Int8Array(size)
		crypto getRandomValues buffer
		ByteString unsafeFromArray buffer.toArray
	}

	private def insecureRandomBytes(size:Int):ByteString	=
		ByteString unsafeFromArray (Array.fill[Byte](size)((Math.random() * 256).toByte))
}
