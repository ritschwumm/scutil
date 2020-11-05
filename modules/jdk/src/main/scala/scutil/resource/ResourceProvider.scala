package scutil.resource

import java.net.URL
import java.io.InputStream
import java.io.IOException

import scutil.core.implicits._
import scutil.io.implicits._
import scutil.lang.ByteString

final class ResourceProvider(val findUrl:String=>Option[URL]) {
	def readByteString(path:String):Option[ByteString]	=
		withInputStream(path, _.readFullyByteString())

	def withInputStream[T](path:String, code:InputStream=>T):Option[T]	=
		openStream(path) map { _ use code }

	def openStream(path:String):Option[InputStream]	=
		findUrl(path) flatMap { url =>
			try { Some(url.openStream()) }
			catch { case e:IOException => None }
		}
}
