package scutil.regex.extension

import java.util.regex.PatternSyntaxException

import scala.reflect.macros.blackbox.Context

private final class RegexMacros(val c:Context) {
	import c.universe._

	def reImpl():c.Tree	=
		c.prefix.tree match {
			case Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	=>
				try {
					str.r
					q"${str}.r"
				}
				catch { case e:PatternSyntaxException =>
					c.abort(
						c.enclosingPosition,
						s"invalid regex literal ${str} at index ${e.getIndex.toString}: ${e.getDescription.toString}"
					)
				}
			case x =>
				c.abort(c.enclosingPosition, s"invalid regex literal ${x.toString}")
		}
}
