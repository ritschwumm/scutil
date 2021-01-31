package scutil.classpath

import java.net.URL
import java.io._
import java.nio.charset.Charset

import scutil.core.implicits._
import scutil.io.implicits._
import scutil.lang._

final case class ClasspathResource(url:URL) {
	def utf8Lines:Seq[String]	=
		lines(Charsets.utf_8)

	def lines(charset:Charset):Seq[String]	=
		withReader(charset)(_.readLines())

	def utf8String:String	=
		string(Charsets.utf_8)

	def string(charset:Charset):String	=
		withReader(charset)(_.readFully())

	def byteString:ByteString	=
		withInputStream(_.readFullyByteString())

	def withReader[T](charset:Charset)(code:Reader=>T):T	=
		withInputStream { stream =>
			new InputStreamReader(stream, charset) use code
		}

	def withInputStream[T](code:InputStream=>T):T	=
		url.openStream() use code
}
