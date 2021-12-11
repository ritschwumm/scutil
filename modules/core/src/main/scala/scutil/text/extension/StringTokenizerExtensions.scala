package scutil.text.extension

import java.util.StringTokenizer

object StringTokenizerExtensions {
	implicit final class StringTokenizerExt(peer:StringTokenizer) {
		def toIterator:Iterator[String]	=
			new Iterator[String] {
				def hasNext	= peer.hasMoreTokens
				def next()	= peer.nextToken()
			}
	}
}
