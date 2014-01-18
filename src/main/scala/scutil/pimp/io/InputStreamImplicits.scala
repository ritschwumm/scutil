package scutil.pimp

import java.io._
import java.nio.charset.Charset

import scala.collection.mutable

object InputStreamImplicits extends InputStreamImplicits

trait InputStreamImplicits {
    implicit def toInputStreamExt(peer:InputStream)	= new InputStreamExt(peer)
}

/** utility methods for InputStream objects */ 
final class InputStreamExt(peer:InputStream) {
	val blockSize	= 16384
	
	/** read as much into the buffer as possible, return how much */
	def readExactly(buffer:Array[Byte]):Int	= {
		val length	= buffer.length
		var offset	= 0
		while (offset < length) {
			val	read	= peer read (buffer, offset, length-offset)
			if (read == -1)	return offset
			offset	+= read
		}
		offset
	}
	
	/** read the complete content */
	def readFully():Array[Byte] = {
		val	buffer	= new Array[Byte](blockSize)
		val out		= new mutable.ArrayBuffer[Byte]
		var	running	= true
		while (running) {
			val len	= peer read buffer
			if (len != -1)	out ++= buffer.view(0, len)
			else			running	= false
		}
		out.toArray
	}
	
	/** skip as much as desired if possible, return how much */
	def skipExactly(count:Long):Long	= {
		var done	= 0L
		while (done < count) {
			val len	= peer skip (count - done)
			if (len == -1)	return done
			done	+= len
		}
		done
	}
	
	/** skip to end */
	def skipFully() {
		val buffer	= new Array[Byte](blockSize)
		var running	= true
		while (running) {
			val len = peer read buffer
			if (len == -1)	running	= false
		}
	}
	
	/** copy everything */
	def transferTo(out:OutputStream) {
		val	buffer	= new Array[Byte](blockSize)
		var running	= true
		while (running) {
			val	len	= peer read buffer
			if (len != -1)	out write (buffer, 0, len)
			else			running	= false
		}
	}
	
	//------------------------------------------------------------------------------
	
	def concat(that:InputStream):InputStream	=
			new SequenceInputStream(peer, that)
		
	def toReader(encoding:Charset):InputStreamReader	=
			new InputStreamReader(peer, encoding)
}