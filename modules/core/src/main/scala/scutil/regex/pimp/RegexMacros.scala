package scutil.regex.pimp

import java.util.regex.PatternSyntaxException

import scala.reflect.macros.blackbox.Context

private final class RegexMacros(val c:Context) {
	import c.universe._
	
	def reImpl():c.Tree	= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		try {
			str.r
			q"${str}.r"
		}
		catch { case e:PatternSyntaxException =>
			c abort (
				c.enclosingPosition,
				s"invalid regex literal ${str} at index ${e.getIndex}: " + e.getDescription
			)
		}
	}
}
