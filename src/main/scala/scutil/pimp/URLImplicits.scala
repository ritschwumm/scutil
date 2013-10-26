package scutil.pimp

import java.io._
import java.nio.charset.Charset
import java.net.URL

import scutil.pimp.AnyImplicits._
import scutil.pimp.DisposableConversions._

object URLImplicits extends URLImplicits

trait URLImplicits {
	implicit def toURLExt(peer:URL)	= new URLExt(peer)
}

final class URLExt(peer:URL) {
	/** execute a closure with an InputStream reading from this URL */
	def withInputStream[T](code:(InputStream=>T)):T	=
			peer.openStream() use code
	
	/** execute a closure with a Reader reading from this URL */
	def withReader[T](charset:Charset)(code:(InputStreamReader=>T)):T	=
			new InputStreamReader(peer.openStream(), charset) use code
		
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
