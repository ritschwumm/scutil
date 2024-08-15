package scutil.io.extension

import java.io.*
import java.nio.charset.Charset

import scutil.lang.ByteString

object InputStreamExtensions {
	private val blockSize	= 16384

	/** utility methods for InputStream objects */
	extension (peer:InputStream) {

		/** read as much into the buffer as possible, return how much */
		def readExactly(buffer:Array[Byte]):Int	=
			peer.readNBytes(buffer, 0, buffer.length)

		/** read as much as possible, up to a given length */
		def readExactlyByteString(length:Int):ByteString	=
			ByteString.unsafeFromArray(peer.readNBytes(length))

		def readLimitedByteString(length:Int):Option[ByteString]	= {
			// NOTE this is copied due the the length-dependent slicing necessary
			val buffer	= new Array[Byte](length)
			val found	= peer.read(buffer)
			if		(found == -1)	None
			else if	(found == 0)	Some(ByteString.empty)
			else					Some(ByteString.unboundedSliceFromArray(buffer, 0, found))
		}

		/** read the complete content */
		def readFullyByteString():ByteString	=
			ByteString.unsafeFromArray(peer.readAllBytes())

		/** skip as much as desired if possible, return how much */
		def skipExactly(count:Long):Long	= {
			// TODO java 12 has skipNBytes
			var done	= 0L
			while (done < count) {
				val len	= peer.skip(count - done)
				if (len == -1)	return done
				done	+= len
			}
			done
		}

		/** skip to end */
		def skipFully():Unit = {
			// TODO java 11 can we use InputStream.skip for this?
			val buffer	= new Array[Byte](blockSize)
			var running	= true
			while (running) {
				val len = peer.read(buffer)
				if (len == -1)	running	= false
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
