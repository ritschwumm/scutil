package scutil.lang.pimp

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

object ByteArrayImplicits extends ByteArrayImplicits

trait ByteArrayImplicits {
	implicit def toByteByteArrayExt(peer:Array[Byte]):ByteArrayExt	= new ByteArrayExt(peer)
}

final class ByteArrayExt(peer:Array[Byte]) {
	def asString(charset:Charset):String	= new String(peer, charset)
	def asUtf8String:String					= asString(StandardCharsets.UTF_8)
}