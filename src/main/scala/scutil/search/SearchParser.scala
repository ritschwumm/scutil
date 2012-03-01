package scutil.search

import scutil.Implicits._

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
		val (negative,positive)	= pattern.trim splitAround ' ' filter { _.nonEmpty } map parseHit partition { _._1 }
		SearchPattern(positive map { _._2 }, negative map { _._2 })
	}
	
	private def parseHit(descriptor1:String):(Boolean,SearchToken) = {
		val (exclude,descriptor2)	= descriptor1 cutPrefix "-" filter { _.nonEmpty} cata ((true,_), (false,descriptor1))
		val (start,descriptor3)		= descriptor2 cutPrefix "|" filter { _.nonEmpty} cata ((true,_), (false,descriptor2))
		val (end,descriptor4)		= descriptor3 cutSuffix "|" filter { _.nonEmpty} cata ((true,_), (false,descriptor3))
		val caseInsensitive			= descriptor4 == descriptor4.toLowerCase
		
		(exclude, SearchToken(descriptor4, caseInsensitive, start, end))
	}
}
