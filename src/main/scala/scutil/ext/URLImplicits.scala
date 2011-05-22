package scutil.ext

import java.io.File
import java.net.URL

object URLImplicits extends URLImplicits

trait URLImplicits {
	implicit def toURLExt(delegate:URL)	= new URLExt(delegate)
}

final class URLExt(delegate:URL) {
	/** 
	converts a "file://..." URL to a File without being too critical
	@see http://www2.java.net/blog/2007/04/25/how-convert-javaneturl-javaiofile
	*/
	def toFile:Option[File]	=
			if (delegate.getProtocol == "file")	Some(
					try { new File(delegate.toURI) } 
					catch { case _	=> new File(delegate.getPath) })
			else None
}