package scutil.search

import java.util.Locale

import scutil.core.implicits.*

/**
selects String who match a pattern with the following syntax:
<br>
<ul>syntax
<li>search consists of substrings in any order</li>
<li>substring searches are ANDed</li>
<li>a substring can be filtered out by prepending it with a '-'</li>
<li>starting a substring with | matches it at the start</li>
<li>ending a substring with | matches it at the end</li>
<li>if a substring contains at least one uppercase character, it is matched case sensitively</li>
</ul>
*/
object SearchParser {
	def parse(pattern:String):SearchPattern	= {
		val (neg, pos)	= pattern.trim.splitAroundChar(' ').filter{ _.nonEmpty }.map(parseHit).partition { _._1 }
		SearchPattern(
			pos.map { _._2 },
			neg.map { _._2 }
		)
	}

	private def parseHit(descriptor:String):(Boolean,SearchToken) = {
		val (exclude,	a)	= scan(descriptor,	_.cutPrefix("-"))
		val (start,		b)	= scan(a,			_.cutPrefix("|"))
		val (end,		c)	= scan(b,			_.cutSuffix("|"))
		// TODO search this should allow unicode
		val noCase			= c == c.toLowerCase(Locale.US)
		(exclude, SearchToken(c, noCase, start, end))
	}

	private def scan(s:String, func:String=>Option[String]):(Boolean,String)	= {
		val t	= func(s).filter(_.nonEmpty).toRight(s)
		(t.isRight, t.merge)
	}
}
