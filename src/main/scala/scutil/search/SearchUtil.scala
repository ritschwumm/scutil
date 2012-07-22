package scutil.search

import scutil.lang._

object SearchUtil {
	def compile(pattern:String):Predicate[String]			= SearchCompiler compile	(SearchParser parse pattern) 
	def compileSeq(pattern:String):Predicate[Seq[String]]	= SearchCompiler compileSeq	(SearchParser parse pattern) 
}
