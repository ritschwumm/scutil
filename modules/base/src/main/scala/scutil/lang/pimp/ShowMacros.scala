package scutil.lang.pimp

import scala.reflect.macros.blackbox.Context

private final class ShowMacros(val c:Context) {
	import c.universe._
	
	def showImpl(args:c.Expr[Any]*):c.Expr[String]	= {
		// literals are a Seq of Literal(Constant(String))
		val Apply(_, List(Apply(_, literals)))	= c.prefix.tree
		val escapeds:Seq[c.Tree]	= literals map { expr => q"""_root_.scala.StringContext.treatEscapes($expr)""" }
			
		val inserts:Seq[c.Tree]		= args map { expr => q"""_root_.scutil.lang.tc.Show.doit($expr)""" }
	
		val parts:Seq[c.Tree]		= (escapeds zip inserts flatMap { case (lit, ins) => Seq(lit, ins) }) :+ q"""${escapeds.last}"""
		val full:c.Tree				= parts reduce { (a, b) => q"""$a + $b""" }
		c.Expr[String](full)
	}
}
