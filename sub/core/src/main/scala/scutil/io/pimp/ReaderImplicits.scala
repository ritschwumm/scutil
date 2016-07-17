package scutil.io.pimp

import java.io._

import scala.collection.mutable

import scutil.lang.ISeq

object ReaderImplicits extends ReaderImplicits

trait ReaderImplicits {
    implicit def toReaderExt(peer:Reader)	= new ReaderExt(peer)
}

/** utility methods for Reader objects */
final class ReaderExt(peer:Reader) {
	val blockSize	= 16384
	
	/** read as much into the buffer as possible, return how much */
	def readExactly(buffer:Array[Char]):Int	= {
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
	def readFully():String = {
		val	buffer	= new Array[Char](blockSize)
		val out		= new StringBuilder
		var	running	= true
		while (running) {
			val len	= peer read buffer
			if (len != -1)	out appendAll (buffer, 0, len)
			else			running	= false
		}
		out.toString
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
		val	buffer	= new Array[Char](blockSize)
		var running	= true
		while (true) {
			val len = peer read buffer
			if (len == -1)	running	= false
		}
	}
	
	/** copy everything */
	def transferTo(out:Writer) {
		val	buffer	= new Array[Char](blockSize)
		var running	= true
		while (running) {
			val	len	= peer read buffer
			if (len != -1)	out write (buffer, 0, len)
			else			running	= false
		}
	}
	
	//------------------------------------------------------------------------------
	
	// BETTER  use a specific line separator
	def readLines():ISeq[String]	= {
		val in		= new BufferedReader(peer)
		val buffer	= new mutable.ArrayBuffer[String]
		var running	= true
		while (running) {
			val	line	= in.readLine
			if (line != null)	buffer	+= line
			else				running	= false
		}
		buffer.toVector
	}
}
