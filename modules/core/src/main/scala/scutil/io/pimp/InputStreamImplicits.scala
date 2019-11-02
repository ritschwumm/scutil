package scutil.io.pimp

import java.io._
import java.nio.charset.Charset

import scutil.lang.ByteString

object InputStreamImplicits extends InputStreamImplicits

trait InputStreamImplicits {
	/** utility methods for InputStream objects */
	implicit final class InputStreamExt(peer:InputStream) {
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

		// NOTE java 11 has readNBytes (in ByteArrayInputStream)
		def readLimitedByteString(length:Int):Option[ByteString]	= {
			val buffer	= new Array[Byte](length)
			val found	= peer read buffer
				 if (found == -1)	None
			else if (found == 0)	Some(ByteString.empty)
			else Some(
				(ByteString makeWithArray found) { out =>
					System arraycopy (buffer, 0, out, 0, found)
				}
			)
		}

		/** read the complete content */
		def readFullyByteString():ByteString	=
				ByteString unsafeFromArray readFullyImpl()

		// NOTE java 11 has readAllBytes
		private def readFullyImpl():Array[Byte] = {
			val baos	= new ByteArrayOutputStream
			transferToPre9(baos)
			baos.flush()
			baos.toByteArray
		}

		// NOTE java 12 has skipNBytes
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
		// TODO kept only for compatibility with java 8, this exists in java 9 as transferTo
		def transferToPre9(out:OutputStream) {
			val	buffer	= new Array[Byte](blockSize)
			var running	= true
			while (running) {
				val	len	= peer read buffer
				if (len != -1)	out write (buffer, 0, len)
				else			running	= false
			}
		}

		//------------------------------------------------------------------------------

		def buffered:BufferedInputStream	=
				new BufferedInputStream(peer)

		def concat(that:InputStream):InputStream	=
				new SequenceInputStream(peer, that)

		def toReader(encoding:Charset):InputStreamReader	=
				new InputStreamReader(peer, encoding)
	}
}
