package scutil.text.pimp

import java.util.StringTokenizer

object StringTokenizerImplicits extends StringTokenizerImplicits

trait StringTokenizerImplicits {
    implicit def toStringTokenizerExt(peer:StringTokenizer)	= new StringTokenizerExt(peer)
}

final class StringTokenizerExt(peer:StringTokenizer) {
	def asIterator:Iterator[String]	=
			new Iterator[String] {
				def hasNext	= peer.hasMoreTokens
				def next	= peer.nextToken
			}
}
