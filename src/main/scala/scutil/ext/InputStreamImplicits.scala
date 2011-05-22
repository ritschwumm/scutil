package scutil.ext

import java.io._

import scala.collection.mutable

object InputStreamImplicits extends InputStreamImplicits

trait InputStreamImplicits {
    implicit def toInputStreamExt(delegate:InputStream)	= new InputStreamExt(delegate)
}

/** utility methods for InputStream objects */ 
final class InputStreamExt(delegate:InputStream) {
	val blockSize	= 16384
	
	def readFully():Array[Byte] = {
		val	buffer	= new Array[Byte](blockSize)
		val out		= new mutable.ArrayBuffer[Byte]
		var	running	= true
		while (running) {
			val len	= delegate read buffer
			if (len != -1)	out ++= buffer.view(0, len)
			else			running	= false
		}
		out.toArray
	}
	
	def copyTo(out:OutputStream) {
		val	buffer	= new Array[Byte](blockSize)
		var running	= true
		while (running) {
			val	len	= delegate read buffer
			if (len != -1)	out write (buffer, 0, len)
			else			running	= false
		}
	}
	
	def drain() {
		val buffer	= new Array[Byte](blockSize)
		var running	= true
		while (running) {
			val len = delegate read buffer
			if (len == -1)	running	= false
		}
	}
}
