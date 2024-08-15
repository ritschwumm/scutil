package scutil.lang.extension

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

import scutil.lang.ByteString

object ByteArrayExtensions {
	extension (peer:Array[Byte]) {
		def asString(charset:Charset):String	= new String(peer, charset)
		def asUtf8String:String					= asString(StandardCharsets.UTF_8)
		def toByteString:ByteString				= ByteString.fromArray(peer)
		def unsafeAsByteString:ByteString		= ByteString.unsafeFromArray(peer)
	}
}
