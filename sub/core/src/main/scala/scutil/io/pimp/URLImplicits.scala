package scutil.io.pimp

import java.io._
import java.nio.charset.Charset
import java.net._

import scutil.lang._
import scutil.lang.implicits._

object URLImplicits extends URLImplicits

trait URLImplicits {
	implicit def toURLExt(peer:URL)	= new URLExt(peer)
}

final class URLExt(peer:URL) {
	/** execute a closure with an InputStream reading from this URL */
	def withInputStream[T](proxy:Option[Proxy])(code:InputStream=>T):T	=
			openConnectionWithOptionalProxy(proxy).getInputStream() use code
	
	/** execute a closure with a Reader reading from this URL */
	def withReader[T](proxy:Option[Proxy], charset:Charset)(code:InputStreamReader=>T):T	=
			new InputStreamReader(openConnectionWithOptionalProxy(proxy).getInputStream(), charset) use code
		
	def openInputStream(proxy:Option[Proxy]):Tried[IOException,InputStream]	=
			Catch.byType[IOException] in openConnectionWithOptionalProxy(proxy).getInputStream()
		
	def openConnectionWithOptionalProxy(proxy:Option[Proxy]):URLConnection	=
			proxy match {
				case Some(proxy)	=> peer openConnection proxy
				case None			=> peer openConnection ()
			}
		
	/**
	converts a "file://..." URL to a File without being too critical
	@see http://www2.java.net/blog/2007/04/25/how-convert-javaneturl-javaiofile
	*/
	def toFile:Option[File]	=
			if (peer.getProtocol == "file")	Some(
					try { new File(peer.toURI) }
					catch { case _:Exception => new File(peer.getPath) })
			else None
}
