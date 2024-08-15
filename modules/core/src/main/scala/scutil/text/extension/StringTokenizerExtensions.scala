package scutil.text.extension

import java.util.StringTokenizer

object StringTokenizerExtensions {
	extension (peer:StringTokenizer) {
		def toIterator:Iterator[String]	=
			new Iterator[String] {
				def hasNext	= peer.hasMoreTokens
				def next()	= peer.nextToken()
			}
	}
}
