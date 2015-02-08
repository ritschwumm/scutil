package scutil.text

import java.util.regex.Pattern

import scala.util.matching.Regex

/** a Regex constructed from a regular Pattern instead of a String */
class PatternRegex(override val pattern:Pattern, groupNames:String*) extends Regex(null, groupNames:_*) {
	def this(regex:String, flags:Int, groupNames:String*)	=
			this(Pattern compile (regex, flags), groupNames:_*)
		
	override def toString	= pattern.toString
}
