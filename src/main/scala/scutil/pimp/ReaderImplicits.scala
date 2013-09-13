package scutil.pimp

import java.io._

import scala.collection.mutable

object ReaderImplicits extends ReaderImplicits

trait ReaderImplicits {
    implicit def toReaderExt(delegate:Reader)	= new ReaderExt(delegate)
}

/** utility methods for Reader objects */ 
final class ReaderExt(delegate:Reader) {
	val blockSize	= 16384
	
	/** read as much into the buffer as possible, return how much */
	def readExactly(buffer:Array[Char]):Int	= {
		val length	= buffer.length
		var offset	= 0
		while (offset < length) {
			val	read	= delegate read (buffer, offset, length-offset)
			if (read == -1)	return offset
			offset	+= read
		}
		offset
	}
	
	/** read the complete content */
	def readFully():String = {
		val	buffer	= new Array[Char](blockSize)
		val out		= new StringBuilder
		var	running	= true
		while (running) {
			val len	= delegate read buffer
			if (len != -1)	out appendAll (buffer, 0, len)
			else			running	= false
		}
		out.toString
	}
	
	/** skip as much as desired if possible, return how much */
	def skipExactly(count:Long):Long	= {
		var done	= 0L
		while (done < count) {
			val len	= delegate skip (count - done)
			if (len == -1)	return done
			done	+= len
		}
		done
	}
	
	/** skip to end */
	def skipFully() {
		val	buffer	= new Array[Char](blockSize)
		var running	= true
		while (true) {
			val len = delegate read buffer
			if (len == -1)	running	= false
		}
	}
	
	/** copy everything */
	def copyTo(out:Writer) {
		val	buffer	= new Array[Char](blockSize)
		var running	= true
		while (running) {
			val	len	= delegate read buffer
			if (len != -1)	out write (buffer, 0, len)
			else			running	= false
		}
	}
	
	//------------------------------------------------------------------------------
	
	// TODO should take a line separator argument
	def readLines():Seq[String]	= {
		val in		= new BufferedReader(delegate)
		val buffer	= new mutable.ArrayBuffer[String]
		var running	= true
		while (running) {
			val	line	= in.readLine
			if (line != null)	buffer	+= line
			else				running	= false
		}
		buffer
	}
}
