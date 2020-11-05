package scutil.text.extension

import java.util.StringTokenizer

object StringTokenizerImplicits extends StringTokenizerImplicits

trait StringTokenizerImplicits {
	implicit final class StringTokenizerExt(peer:StringTokenizer) {
		def toIterator:Iterator[String]	=
			new Iterator[String] {
				def hasNext	= peer.hasMoreTokens
				def next()	= peer.nextToken()
			}
	}
}
