package scutil.regex

import java.util.regex.Pattern

import scala.util.matching.Regex

object PatternRegex {
	def compile(regex:String, flags:Int, groupNames:String*):PatternRegex	=
		new PatternRegex(Pattern.compile(regex, flags), groupNames*)
}

/** a Regex constructed from a regular Pattern instead of a String */
class PatternRegex(override val pattern:Pattern, groupNames:String*) extends Regex(null, groupNames*) {
	override def toString:String	= pattern.toString
}
