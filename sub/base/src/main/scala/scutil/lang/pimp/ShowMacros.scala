package scutil.lang.pimp

import scala.reflect.macros.blackbox.Context

private final class ShowMacros(val c:Context) {
	import c.universe._
	
	def showImpl(args:c.Expr[Any]*):c.Expr[String]	= {
		val Apply(_, List(Apply(_, literals)))	= c.prefix.tree
		// literals are a Seq of Literal(Constant(String))
		val inserts:Seq[c.Tree]	= args map { expr => q"""_root_.scutil.lang.tc.Show.doit($expr)""" }
		val parts:Seq[c.Tree]	= (literals zip inserts flatMap { case (lit, ins) => Seq(lit, ins) }) :+ q"""${literals.last}"""
		val full:c.Tree			= parts reduce { (a, b) => q"""$a + $b""" }
		c.Expr[String](full)
	}
}
