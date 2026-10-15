package scutil.io.extension

import java.io.*

import scala.collection.mutable

object ReaderExtensions {
	private val blockSize	= 16384

	extension (peer:Reader) {

		/** read as much into the buffer as possible, return how much */
		def readExactly(buffer:Array[Char]):Int	= {
			val length	= buffer.length
			var offset	= 0
			while offset < length do {
				val	read	= peer.read(buffer, offset, length-offset)
				if read == -1	then return offset
				offset	+= read
			}
			offset
		}

		def readLimited(length:Int):Option[String]	= {
			val	buffer	= new Array[Char](length)
			val found	= peer.read(buffer)
			if		found == -1	then	None
			else if	found == 0	then	Some("")
			else						Some(new String(buffer, 0, found))
		}

		/** read the complete content */
		def readFully():String = {
			val	buffer	= new Array[Char](blockSize)
			val out		= new StringBuilder
			var	running	= true
			while running do {
				val	len	= peer.read(buffer)
				if len != -1
				then out.appendAll(buffer, 0, len)
				else running	= false
			}
			out.toString
		}

		/** skip as much as desired if possible, return how much */
		def skipExactly(count:Long):Long	= {
			var done	= 0L
			while done < count do {
				val len	= peer.skip(count - done)
				if len == -1	then return done
				done	+= len
			}
			done
		}

		/** skip to end */
		def skipFully():Unit = {
			val	buffer	= new Array[Char](blockSize)
			var running	= true
			while running do {
				val len = peer.read(buffer)
				if len == -1	then running	= false
			}
		}

		//------------------------------------------------------------------------------

		// BETTER  use a specific line separator
		def readLines():Seq[String]	= {
			val in		= new BufferedReader(peer)
			val buffer	= new mutable.ArrayBuffer[String]
			var running	= true
			while running do {
				val	line	= in.readLine
				if line != null
				then buffer	+= line
				else running	= false
			}
			buffer.toVector
		}

		//------------------------------------------------------------------------------

		def buffered:BufferedReader	= new BufferedReader(peer)
	}
}
