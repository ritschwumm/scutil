package scutil.pimp

import java.util.StringTokenizer

object StringTokenizerImplicits extends StringTokenizerImplicits

trait StringTokenizerImplicits {
    implicit def toStringTokenizerExt(delegate:StringTokenizer)	= new StringTokenizerExt(delegate)
}

final class StringTokenizerExt(delegate:StringTokenizer) {
	def asIterator:Iterator[String]	=
			new Iterator[String] {
				def hasNext	= delegate.hasMoreTokens
				def next	= delegate.nextToken
			}
}
