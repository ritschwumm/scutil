package scutil.color.pimp

import scala.reflect.macros.blackbox.Context

import scutil.color._

private final class HexColorMacros(val c:Context) {
	import c.universe._

	def rgbImpl():c.Tree	= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		RGB parseHex str match {
			case Some(RGB(r, g, b))	=>
				q"_root_.scutil.color.RGB($r, $g, $b)"
			case None	=>
				c abort (c.enclosingPosition, s"invalid rgb literal ${str}")
		}
	}

	def rgbaImpl():c.Tree	= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		RGBA parseHex str match {
			case Some(RGBA(RGB(r, g, b),Alpha(a)))	=>
				q"_root_.scutil.color.RGBA(scutil.color.RGB($r, $g, $b), scutil.color.Alpha($a))"
			case None	=>
				c abort (c.enclosingPosition, s"invalid rgba literal ${str}")
		}
	}
}
